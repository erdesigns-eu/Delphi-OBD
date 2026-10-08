#!/usr/bin/env python3
"""Read a player log and say how long each channel took.

The engine writes a timestamped line for every step of opening a channel
and one line for the whole stop. Read together they say where a channel
change spent its time - but only if somebody reads them together, which
by hand means subtracting clock times off a screen and is why three runs
went by before a slowdown was noticed.

This does the subtracting. Point it at a log:

    python3 tools/playerlog.py player.log

Write down what a build does, so a later build can be held against it:

    python3 tools/playerlog.py player.log --save baseline.json
    python3 tools/playerlog.py newer.log --against baseline.json

Held against a baseline it prints only what moved, and exits 1 when
something got slower by more than the margin, so it can be run without
anybody reading the output.
"""

from __future__ import annotations

import argparse
import json
import re
import statistics
import sys
from pathlib import Path

# The engine writes the same line in two places, and either can be pasted
# here: the one kept over the picture, which is the time of day and the
# text, and the one in the studio's log file, which puts the date in front
# and the area and thread between. Lines from any other area of the studio
# are somebody else's and are passed over.
LINE = re.compile(
    r"^\s*(?:\d{4}-\d{2}-\d{2}\s+)?"
    r"(\d{2}):(\d{2}):(\d{2})\.(\d{3})\s+"
    r"(?:(\S+)\s+\[\s*\d+\]\s+)?(.*)$")

DAY_MS = 24 * 60 * 60 * 1000

# The steps of opening, in the order they happen. Each is a name and what
# the line starts with; the first line matching a step marks it, so a step
# that repeats (pictures settling again mid-channel) is counted once.
STEPS = [
    ("bytes", "Source: "),
    ("ring", "Ring of "),
    ("tracks", "Sound track"),
    ("sound", "Sound: "),
    ("pictures", "Pictures: "),
    ("shown", "Pictures settle at"),
    ("playing", "State: Playing"),
]

OPENING = re.compile(r"^Opening (live )?(.*)$")
STOPPED = re.compile(r"^Stopped in (\d+) ms: (.*)$")
GAVEUP = re.compile(r"^Gave up opening after (\d+) s")
PART = re.compile(r"([a-z]+) (\d+)")

# What is worth reporting on, and the order to report it in.
COLUMNS = ["stop", "bytes", "ring", "tracks", "pictures", "sound",
           "shown", "playing", "switch"]


def stamp(hh: str, nn: str, ss: str, zzz: str) -> int:
    """The time of day in milliseconds."""
    return ((int(hh) * 60 + int(nn)) * 60 + int(ss)) * 1000 + int(zzz)


def since(began: int, now: int) -> int:
    """How long between two times of day, allowing for midnight."""
    gap = now - began
    if gap < 0:
        gap += DAY_MS
    return gap


def read(text: str) -> list[dict]:
    """Every channel opening in the log, with what each step cost."""
    opens: list[dict] = []
    current: dict | None = None
    pending_stop: dict | None = None
    for raw in text.splitlines():
        found = LINE.match(raw)
        if not found:
            continue
        hh, nn, ss, zzz, area, said = found.groups()
        if area is not None and area != "player":
            continue
        at = stamp(hh, nn, ss, zzz)

        stop = STOPPED.match(said)
        if stop:
            pending_stop = {
                "at": at,
                "ms": int(stop.group(1)),
                "parts": {name: int(cost)
                          for name, cost in PART.findall(stop.group(2))},
            }
            current = None
            continue

        opening = OPENING.match(said)
        if opening:
            current = {
                "at": at,
                "live": bool(opening.group(1)),
                "what": opening.group(2),
                "steps": {},
                "gave up": None,
                # A stop that ran straight into this opening is part of the
                # same channel change; one from minutes ago is not.
                "stop": pending_stop["ms"]
                        if pending_stop and since(pending_stop["at"], at) < 5000
                        else None,
            }
            opens.append(current)
            pending_stop = None
            continue

        if current is None:
            continue

        gave = GAVEUP.match(said)
        if gave:
            current["gave up"] = int(gave.group(1)) * 1000
            current = None
            continue

        if said.startswith("State: ") and said != "State: Playing":
            continue

        for name, starts in STEPS:
            if said.startswith(starts) and name not in current["steps"]:
                current["steps"][name] = since(current["at"], at)
                break

    for one in opens:
        shown = one["steps"].get("shown")
        if shown is not None:
            # The old channel being let go of, then the new one getting its
            # pictures' size settled. That last step is the end of setting up
            # and not the picture itself, so this reads a few hundred
            # milliseconds short of what somebody sitting in front of it
            # waits; "playing" is the honest one, and is kept beside it.
            one["steps"]["switch"] = shown + (one["stop"] or 0)
        if one["stop"] is not None:
            one["steps"]["stop"] = one["stop"]
    return opens


def words(ms: int | None) -> str:
    if ms is None:
        return "-"
    if ms >= 10000:
        return f"{ms / 1000:.1f} s"
    return f"{ms} ms"


def figures(opens: list[dict]) -> dict[str, dict[str, float]]:
    """The middle and the edges of each step across every opening.

    An opening that gave up is left out: it says how long the giving up
    takes, which is a fixed number, not how fast the player is."""
    played = [one for one in opens if one["gave up"] is None]
    out: dict[str, dict[str, float]] = {}
    for name in COLUMNS:
        got = [one["steps"][name] for one in played if name in one["steps"]]
        if got:
            out[name] = {
                "count": len(got),
                "least": min(got),
                "middle": statistics.median(got),
                "most": max(got),
            }
    return out


def report(opens: list[dict]) -> None:
    if not opens:
        print("No channel openings in this log.")
        return
    width = max(len(name) for name in COLUMNS) + 2
    print(f"{len(opens)} openings\n")
    for number, one in enumerate(opens, 1):
        head = one["what"] if one["what"] else "(no address)"
        print(f"{number:>3}. {head}")
        # In the order they happened, which is not always the order they
        # are listed in - the sound being ready before the pictures, or
        # the other way about, is itself worth seeing.
        for name, cost in sorted(one["steps"].items(), key=lambda step: step[1]):
            print(f"     {name:<{width}}{words(cost)}")
        if one["gave up"] is not None:
            print(f"     {'gave up':<{width}}{words(one['gave up'])}")
        if not one["steps"].get("shown") and one["gave up"] is None:
            print(f"     {'no picture':<{width}}")
        print()

    seen = figures(opens)
    if not seen:
        return
    gave = sum(1 for one in opens if one["gave up"] is not None)
    if gave:
        print(f"Across all of them, less the {gave} that gave up")
    else:
        print("Across all of them")
    print(f"     {'':<{width}}{'least':>9}{'middle':>10}{'most':>10}{'of':>5}")
    for name in COLUMNS:
        if name in seen:
            row = seen[name]
            print(f"     {name:<{width}}"
                  f"{words(int(row['least'])):>9}"
                  f"{words(int(row['middle'])):>10}"
                  f"{words(int(row['most'])):>10}"
                  f"{row['count']:>5}")


def against(seen: dict, was: dict, margin: float) -> int:
    """Hold what this log did against what a baseline said. Returns how
    many steps got worse by more than the margin."""
    worse = 0
    print(f"{'':<12}{'was':>10}{'now':>10}{'':>4}")
    for name in COLUMNS:
        if name not in seen or name not in was:
            continue
        before = was[name]["middle"]
        after = seen[name]["middle"]
        if before <= 0:
            continue
        moved = (after - before) / before
        if abs(moved) < margin:
            mark = ""
        elif moved > 0:
            mark = f"  slower by {moved * 100:.0f}%"
            worse += 1
        else:
            mark = f"  faster by {-moved * 100:.0f}%"
        print(f"{name:<12}{words(int(before)):>10}{words(int(after)):>10}{mark}")
    return worse


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("log", nargs="?",
        help="the log file; leave it out to read what is piped in")
    parser.add_argument("--save", metavar="FILE",
        help="write what this log did, to hold a later one against")
    parser.add_argument("--against", metavar="FILE",
        help="hold this log against one written earlier")
    parser.add_argument("--margin", type=float, default=0.2,
        help="how much a step may move before it is worth saying (0.2)")
    args = parser.parse_args()

    if args.log:
        text = Path(args.log).read_text(encoding="utf-8", errors="replace")
    else:
        text = sys.stdin.read()

    opens = read(text)
    seen = figures(opens)

    if args.against:
        was = json.loads(Path(args.against).read_text(encoding="utf-8"))
        if not seen:
            print("No channel openings in this log; nothing to hold against.")
            return 1
        worse = against(seen, was.get("steps", {}), args.margin)
        if worse:
            print(f"\n{worse} step(s) slower than the baseline.")
        else:
            print("\nNothing slower than the baseline.")
        return 1 if worse else 0

    report(opens)

    if args.save:
        Path(args.save).write_text(
            json.dumps({"openings": len(opens), "steps": seen},
                       indent=4, ensure_ascii=False) + "\n",
            encoding="utf-8")
        print(f"\nWritten to {args.save}")
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except BrokenPipeError:
        # Piped into something that stopped reading - head, or a pager
        # somebody quit out of. Not a fault worth a stack trace.
        sys.exit(0)
