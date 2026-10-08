#!/usr/bin/env python3
"""Run every checker and report what each one found.

    python tools/pascalcheck/run.py            # one line per checker
    python tools/pascalcheck/run.py -v         # plus the findings themselves
    python tools/pascalcheck/run.py capture    # only checkers matching a name

Exits non-zero when anything was found, so it can stand in a build step.
"""
import argparse, os, re, subprocess, sys
from concurrent.futures import ThreadPoolExecutor

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
from common import ROOT
# A checker prints a heading, its findings, then a total. These are the shapes
# that total comes in.
TOTAL = re.compile(r'^\s*(?:total(?: distinct)?|mismatches):\s*(\d+)\s*$', re.M)
INLINE = re.compile(r'\bmismatches:\s*(\d+)\s*$|^--\s*(\d+) finding', re.M)


def count(output):
    """How many findings the output reports, or None when it says nothing."""
    hits = TOTAL.findall(output)
    if not hits:
        hits = [h for pair in INLINE.findall(output) for h in pair if h]
    if not hits:
        # A checker that printed nothing at all found nothing at all.
        return 0 if not output.strip() else None
    return sum(int(h) for h in hits)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('pattern', nargs='?', default='',
                    help='only run checkers whose name contains this')
    ap.add_argument('-v', '--verbose', action='store_true',
                    help='print the findings, not just the counts')
    ap.add_argument('-j', '--jobs', type=int, default=min(4, os.cpu_count() or 1),
                    help='independent checker processes (default up to four)')
    ap.add_argument('--errors-only', action='store_true',
                    help='report compiler hints/warnings but fail only on other findings')
    args = ap.parse_args()
    if args.jobs < 1:
        ap.error('--jobs must be positive')

    names = sorted(f for f in os.listdir(HERE)
                   if f.startswith('check_') and f.endswith('.py')
                   and args.pattern in f)
    if not names:
        print('no checker matches %r' % args.pattern)
        return 2

    # These imported application-specific checks need inputs this library does
    # not ship. Report them as skipped, never as a successful validation.
    requirements = {
        'constkey': ('translations/en.json',),
        'i18n': ('translations/en.json',),
        'i18nmissing': ('translations/en.json',),
        'i18norphan': ('translations/en.json',),
        'wraparound': ('units/BigNumbers.pas', 'units/ChaChaPoly.pas',
                       'units/Curve25519.pas', 'units/SrpClient.pas'),
    }
    skipped = []
    found = {}
    unclear = []
    def run_checker(name):
        return subprocess.run([sys.executable, os.path.join(HERE, name)],
                              capture_output=True, text=True, cwd=ROOT)

    pending = {}
    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        for name in names:
            label = name[len('check_'):-len('.py')]
            inputs = requirements.get(label, ())
            if not inputs or any(os.path.exists(os.path.join(ROOT, p)) for p in inputs):
                pending[name] = pool.submit(run_checker, name)
    for name in names:
        label = name[len('check_'):-len('.py')]
        inputs = requirements.get(label, ())
        if inputs and not any(os.path.exists(os.path.join(ROOT, p)) for p in inputs):
            print('%-18s SKIP (project-specific inputs absent)' % label)
            skipped.append(label)
            continue
        r = pending[name].result()
        out = (r.stdout or '') + (r.stderr or '')
        label = name[len('check_'):-len('.py')]
        n = count(out)
        if r.returncode != 0 and not (r.returncode == 1 and n is not None and n > 0):
            print('%-18s ERROR' % label)
            print(out.rstrip())
            unclear.append(label)
            continue
        n = count(out)
        if n is None:
            print('%-18s ?  (no total line)' % label)
            unclear.append(label)
        elif n:
            print('%-18s %d' % (label, n))
            found[label] = n
        else:
            print('%-18s .' % label)
        if args.verbose and (n or n is None):
            print(out.rstrip())
            print()

    print()
    if found:
        print('%d finding(s) across %d checker(s): %s' %
              (sum(found.values()), len(found), ', '.join(sorted(found))))
    else:
        print('clean across %d executed checkers' % (len(names) - len(skipped)))
    if skipped:
        print('skipped: %s' % ', '.join(skipped))
    if unclear:
        print('could not read a total from: %s' % ', '.join(unclear))
    advisory = {'private', 'unused', 'inlineunit', 'hidden'}
    blocking = set(found) - advisory if args.errors_only else set(found)
    if args.errors_only and set(found) & advisory:
        print('advisory only: %s' % ', '.join(sorted(set(found) & advisory)))
    return 1 if blocking or unclear else 0


if __name__ == '__main__':
    sys.exit(main())
