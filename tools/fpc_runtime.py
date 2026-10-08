#!/usr/bin/env python3
"""Compile every Linux nonvisual unit and run real FPC runtime regressions.

Requires FPC 3.3.1+ with its RTL, FCL and vcl-compat packages. No API stubs.
"""
import argparse
import pathlib
import shutil
import socket
import subprocess
import tempfile
import threading

ROOT = pathlib.Path(__file__).resolve().parents[1]
# These existing backends require a Windows SDK or Delphi Bluetooth framework.
PLATFORM_UNITS = {
    'ERD.Connection.Bluetooth', 'ERD.Connection.BLE',
    'ERD.Connection.FTDI', 'ERD.Connection.Serial',
    'ERD.J2534', 'ERD.J2534.Components',
    'ERD.Protocol.KWP1281.Transport.J2534',
    'ERD.Protocol.KWP1281.Transport.Serial',
}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--compiler', default=shutil.which('fpc'))
    parser.add_argument('--source-tree', type=pathlib.Path,
                        help='Built official FPC source tree (compiler, rtl, packages)')
    parser.add_argument('--rtl', type=pathlib.Path,
                        help='Installed units/<target> containing RTL and package subdirectories')
    parser.add_argument('--runtime-only', action='store_true',
                        help='Run linked regressions only; omit full-library compilation')
    args = parser.parse_args()
    compiler = args.compiler
    if not compiler and args.source_tree:
        compiler = str(args.source_tree / 'compiler/ppcx64')
    if not compiler:
        parser.error('pass --compiler or --source-tree')
    version = subprocess.check_output([compiler, '-iV'], text=True).strip()
    if tuple(map(int, version.split('.'))) < (3, 3, 1):
        parser.error('full runtime requires FPC 3.3.1+; use fpc_smoke.py for 3.2.2 codecs')
    target_os = subprocess.check_output([compiler, '-iTO'], text=True).strip().lower()
    if target_os != 'linux':
        parser.error('this validation profile targets Linux; Windows backends require a separate build')
    target = subprocess.check_output([compiler, '-iTP'], text=True).strip() + '-' + target_os
    if args.source_tree:
        packages = [args.source_tree / 'rtl/units' / target]
        packages += sorted((args.source_tree / 'packages').glob('*/units/' + target))
    elif args.rtl:
        packages = sorted(p for p in args.rtl.iterdir() if p.is_dir())
    else:
        packages = []  # installed compiler's own fpc.cfg resolves packages
    sources = sorted((ROOT / 'src').rglob('*.pas'))
    units = [p for p in sources if p.parent.name not in ('UI', 'UI.FMX', 'DesignTime')
             and p.stem != 'HEADER.template' and p.stem not in PLATFORM_UNITS]
    with tempfile.TemporaryDirectory(prefix='erd-runtime-') as directory:
        output = pathlib.Path(directory)
        flags = ['-Mdelphi', '-FU' + directory, '-FE' + directory]
        if packages:
            flags += ['-n']
        flags += ['-Fu' + str(p) for p in packages]
        flags += ['-Fu' + str(p) for p in sorted({p.parent for p in sources})]
        failures = []
        for unit in ([] if args.runtime_only else units):
            result = subprocess.run([compiler, *flags, '-Cn', str(unit)],
                                    capture_output=True, text=True)
            if result.returncode:
                failures.append(unit.stem)
                print(result.stdout + result.stderr)
        if failures:
            raise SystemExit('Compilation failed: ' + ', '.join(failures))
        if not args.runtime_only:
            print(f'FPC {version}: {len(units)} Linux nonvisual units compiled.', flush=True)
        else:
            print('Runtime regressions only; full-library compilation omitted.', flush=True)
        print('Platform-specific backends outside this target: ' + ', '.join(sorted(PLATFORM_UNITS)))
        runner = output / 'Runtime.dpr'
        shutil.copyfile(ROOT / 'tools/fpc-smoke/Runtime.dpr', runner)
        result = subprocess.run([compiler, *flags, '-gl', str(runner)], capture_output=True, text=True)
        if result.returncode:
            raise SystemExit(result.stdout + result.stderr)
        # The peer deliberately sends less than the reader's 1024-byte buffer.
        with socket.socket() as server, socket.socket() as stalled_server, socket.socket(type=socket.SOCK_DGRAM) as udp:
            udp.bind(('127.0.0.1', 0))
            server.bind(('127.0.0.1', 0))
            server.listen(1)
            stalled_server.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, 1024)
            stalled_server.bind(('127.0.0.1', 0))
            stalled_server.listen(1)
            stalled_done = threading.Event()
            errors = []
            def peer():
                try:
                    server.settimeout(10)
                    conn, _ = server.accept()
                    with conn:
                        conn.settimeout(5)
                        if conn.recv(4) != b'PING':
                            raise AssertionError('Unexpected TCP request')
                        conn.sendall(b'OK>')
                        # Keep connection open: WAITALL would hang until timeout.
                        conn.recv(1)
                except Exception as exc:
                    errors.append(exc)
            def udp_peer():
                try:
                    udp.settimeout(10)
                    data, endpoint = udp.recvfrom(1024)
                    if data != bytes((0, 128, 255)):
                        raise AssertionError('Unexpected UDP datagram')
                    udp.sendto(data, endpoint)
                except Exception as exc:
                    errors.append(exc)
            def stalled_peer():
                try:
                    stalled_server.settimeout(10)
                    conn, _ = stalled_server.accept()
                    with conn:
                        # Deliberately never drain the TCP receive window.
                        stalled_done.wait(40)
                except Exception as exc:
                    errors.append(exc)
            stalled_worker = threading.Thread(target=stalled_peer, daemon=True)
            stalled_worker.start()
            udp_worker = threading.Thread(target=udp_peer, daemon=True)
            udp_worker.start()
            worker = threading.Thread(target=peer, daemon=True)
            worker.start()
            subprocess.run([str(output / 'Runtime'), directory, str(server.getsockname()[1]), str(udp.getsockname()[1]), str(ROOT / 'catalogs'), str(stalled_server.getsockname()[1])],
                           check=True, timeout=30)
            stalled_done.set()
            stalled_worker.join(timeout=10)
            worker.join(timeout=10)
            udp_worker.join(timeout=10)
            if worker.is_alive() or udp_worker.is_alive() or stalled_worker.is_alive() or errors:
                raise SystemExit('Network fixture failed: ' + repr(errors))

        tls_runner = output / 'TLS.dpr'
        shutil.copyfile(ROOT / 'tools/fpc-smoke/TLS.dpr', tls_runner)
        result = subprocess.run([compiler, *flags, '-gl', str(tls_runner)], capture_output=True, text=True)
        if result.returncode:
            raise SystemExit(result.stdout + result.stderr)
        import ssl
        def openssl(*arguments):
            subprocess.run(['openssl', *arguments], check=True, capture_output=True)
        key = output / 'server.key'
        csr = output / 'server.csr'
        openssl('req', '-new', '-newkey', 'rsa:2048', '-nodes', '-keyout', str(key),
                '-out', str(csr), '-subj', '/CN=localhost', '-addext',
                'subjectAltName=DNS:localhost,IP:127.0.0.1')
        message, public_key, signature = (output / 'message.bin', output / 'public.pem', output / 'signature.bin')
        message.write_bytes(b'ERD firmware signature regression')
        openssl('pkey', '-in', str(key), '-pubout', '-out', str(public_key))
        openssl('dgst', '-sha256', '-sign', str(key), '-out', str(signature), str(message))
        crypto_runner = output / 'Crypto.dpr'
        shutil.copyfile(ROOT / 'tools/fpc-smoke/Crypto.dpr', crypto_runner)
        result = subprocess.run([compiler, *flags, '-gl', str(crypto_runner)], capture_output=True, text=True)
        if result.returncode:
            raise SystemExit(result.stdout + result.stderr)
        subprocess.run([str(output / 'Crypto'), str(message), str(public_key), str(signature)], check=True, timeout=10)
        good = output / 'good.pem'
        expired = output / 'expired.pem'
        for cert, days in [(good, '1')]:
            openssl('x509', '-req', '-in', str(csr), '-signkey', str(key),
                    '-out', str(cert), '-days', days, '-copy_extensions', 'copy')
        (output / 'index').write_text('')
        (output / 'serial').write_text('01\n')
        config = output / 'ca.cnf'
        config.write_text(f"[ca]\ndefault_ca=local\n[local]\ndatabase={output}/index\nnew_certs_dir={output}\nserial={output}/serial\nprivate_key={key}\ncertificate={good}\ndefault_md=sha256\npolicy=any\ncopy_extensions=copy\n[any]\ncommonName=supplied\n")
        openssl('ca', '-batch', '-selfsign', '-config', str(config), '-in', str(csr),
                '-startdate', '20200101000000Z', '-enddate', '20200102000000Z', '-out', str(expired), '-notext')
        wrong = output / 'wrong.pem'
        openssl('req', '-x509', '-new', '-key', str(key), '-out', str(wrong),
                '-days', '1', '-subj', '/CN=wrong.example', '-addext', 'subjectAltName=DNS:wrong.example')
        cases = [
            ('allow-self-signed/IP-match', good, '127.0.0.1', 1, '-', 'accept'),
            ('allow-self-signed/DNS-match', good, 'localhost', 1, '-', 'accept'),
            ('allow-self-signed/mismatch', wrong, '127.0.0.1', 1, '-', 'reject'),
            ('allow-self-signed/expired', expired, '127.0.0.1', 1, '-', 'reject'),
            ('require/untrusted', good, '127.0.0.1', 0, '-', 'reject'),
            ('require/trusted', good, '127.0.0.1', 0, str(good), 'accept'),
            ('require/mismatch', wrong, '127.0.0.1', 0, str(wrong), 'reject'),
        ]
        for label, cert, host, mode, ca, expected in cases:
            context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
            context.load_cert_chain(cert, key)
            with socket.socket() as listener:
                listener.bind(('127.0.0.1', 0)); listener.listen(1); listener.settimeout(5)
                def tls_peer():
                    try:
                        conn, _ = listener.accept()
                        with context.wrap_socket(conn, server_side=True) as secured:
                            secured.settimeout(3); secured.recv(1)
                    except (ssl.SSLError, OSError):
                        pass  # Expected when the client's certificate policy rejects the peer.
                peer_thread = threading.Thread(target=tls_peer, daemon=True)
                peer_thread.start()
                subprocess.run([str(output / 'TLS'), host, str(mode), str(listener.getsockname()[1]), ca, expected],
                               check=True, timeout=10, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                peer_thread.join(timeout=5)
                if peer_thread.is_alive():
                    raise SystemExit('TLS peer failed to close: ' + label)
                print('TLS regression passed: ' + label)


if __name__ == '__main__':
    main()
