#!/usr/bin/env python3
"""Compile and run portable codecs with FPC; never substitute Delphi stubs."""
import argparse
import pathlib
import shutil
import subprocess
import tempfile

ROOT = pathlib.Path(__file__).resolve().parents[1]
SOURCES = (
    'src/Core/OBD.Types.pas', 'src/Core/OBD.Version.pas',
    'src/Core/OBD.Binary.Value.pas',
    'src/Core/OBD.CAN.Route.pas', 'src/Protocol/OBD.Protocol.Types.pas',
    'src/Service/OBD.Service.EVBattery.Types.pas',
    'src/Service/OBD.Service.EVBattery.Request.pas',
    'src/Core/OBD.Errors.pas', 'src/Protocol/OBD.Protocol.LIN.Frame.pas',
    'src/Protocol/OBD.Protocol.MOST.Control.pas',
    'src/Protocol/OBD.Protocol.FlexRay.Frame.pas',
)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--compiler', default=shutil.which('fpc') or shutil.which('ppcx64'))
    parser.add_argument('--rtl', type=pathlib.Path,
                        help='FPC units directory containing rtl and rtl-objpas')
    args = parser.parse_args()
    if not args.compiler:
        parser.error('install FPC or pass --compiler')
    compiler = pathlib.Path(args.compiler).resolve()
    rtl = args.rtl
    if rtl is None:
        candidates = list((compiler.parent / 'units').glob('*'))
        if len(candidates) == 1:
            rtl = candidates[0]
    flags = ['-Mdelphi', '-B', '-gl']
    if rtl:
        # Extracted Debian packages do not have a system fpc.cfg; use their
        # genuine RTL units, without fake System.* wrappers or API stubs.
        flags += ['-n', '-Fu' + str(rtl / 'rtl'), '-Fu' + str(rtl / 'rtl-objpas')]
    subprocess.run([str(compiler), '-iV'], check=True)
    with tempfile.TemporaryDirectory(prefix='delphi-obd-fpc-') as directory:
        target = pathlib.Path(directory)
        # Version has no RTL dependency and is compiled unmodified first.
        subprocess.run([str(compiler), *flags, '-FU' + directory,
                        str(ROOT / 'src/Core/OBD.Version.pas')], check=True)
        for relative in SOURCES:
            original = ROOT / relative
            text = original.read_text(encoding='utf-8')
            # Only unit scope names differ for these portable sources.
            text = text.replace('System.SysUtils', 'SysUtils').replace('System.Variants', 'Variants').replace('System.Classes', 'Classes')
            (target / original.name).write_text(text, encoding='utf-8')
        shutil.copyfile(ROOT / 'tools/fpc-smoke/Smoke.dpr', target / 'Smoke.dpr')
        subprocess.run([str(compiler), *flags, '-Fu' + directory, '-FU' + directory,
                        '-FE' + directory, str(target / 'Smoke.dpr')], check=True)
        subprocess.run([str(target / 'Smoke')], check=True)
    print('Portable codec smoke passed; Delphi/VCL/FMX/DUnitX remain untested by FPC.')


if __name__ == '__main__':
    main()
