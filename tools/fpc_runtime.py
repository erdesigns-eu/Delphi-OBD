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
        with socket.socket() as server, socket.socket(type=socket.SOCK_DGRAM) as udp:
            udp.bind(('127.0.0.1', 0))
            server.bind(('127.0.0.1', 0))
            server.listen(1)
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
            udp_worker = threading.Thread(target=udp_peer, daemon=True)
            udp_worker.start()
            worker = threading.Thread(target=peer, daemon=True)
            worker.start()
            subprocess.run([str(output / 'Runtime'), directory, str(server.getsockname()[1]), str(udp.getsockname()[1])],
                           check=True, timeout=30)
            worker.join(timeout=10)
            udp_worker.join(timeout=10)
            if worker.is_alive() or udp_worker.is_alive() or errors:
                raise SystemExit('Network fixture failed: ' + repr(errors))


if __name__ == '__main__':
    main()
