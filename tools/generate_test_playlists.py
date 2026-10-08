#!/usr/bin/env python3
"""Generate deterministic playlist fixtures without storing large files in Git."""

from __future__ import annotations

import argparse
import json
from pathlib import Path


def stream_name(index: int) -> str:
    """Return the deterministic display name for a generated stream."""
    return f"Channel {index:08d}"


def stream_url(index: int, providers: int) -> str:
    """Return a deterministic Xtream-style URL for a generated stream."""
    provider = index % providers
    return f"http://provider-{provider}.invalid/live/user{provider}/pass{provider}/{index}.ts"


def write_m3u(path: Path, streams: int, groups: int, providers: int) -> None:
    """Write an extended M3U fixture directly to disk using bounded memory."""
    with path.open("w", encoding="utf-8", newline="\n") as output:
        output.write("#EXTM3U x-tvg-url=\"https://epg.invalid/guide.xml\"\n")
        for index in range(streams):
            group = index % groups
            output.write(
                f'#EXTINF:-1 tvg-id="channel-{index}" tvg-name="{stream_name(index)}" '
                f'tvg-logo="https://images.invalid/{index}.png" group-title="Group {group:05d}",'
                f"{stream_name(index)}\n"
            )
            if index % 10 == 0:
                output.write("#EXTVLCOPT:http-user-agent=IPTV-M3U-Editor fixture\n")
            output.write(f"{stream_url(index, providers)}\n")


def write_pls(path: Path, streams: int, providers: int) -> None:
    """Write a deterministic PLS fixture."""
    with path.open("w", encoding="utf-8", newline="\n") as output:
        output.write("[playlist]\n")
        for index in range(streams):
            item = index + 1
            output.write(f"File{item}={stream_url(index, providers)}\n")
            output.write(f"Title{item}={stream_name(index)}\n")
            output.write(f"Length{item}=-1\n")
        output.write(f"NumberOfEntries={streams}\nVersion=2\n")


def write_xspf(path: Path, streams: int, providers: int) -> None:
    """Write a deterministic XSPF fixture."""
    with path.open("w", encoding="utf-8", newline="\n") as output:
        output.write('<?xml version="1.0" encoding="UTF-8"?>\n')
        output.write('<playlist version="1" xmlns="http://xspf.org/ns/0/"><trackList>\n')
        for index in range(streams):
            output.write("<track>")
            output.write(f"<title>{stream_name(index)}</title>")
            output.write(f"<location>{stream_url(index, providers)}</location>")
            output.write("</track>\n")
        output.write("</trackList></playlist>\n")


def write_asx(path: Path, streams: int, providers: int) -> None:
    """Write a deterministic ASX fixture."""
    with path.open("w", encoding="utf-8", newline="\n") as output:
        output.write('<asx version="3.0">\n')
        for index in range(streams):
            output.write("<entry>")
            output.write(f"<title>{stream_name(index)}</title>")
            output.write(f'<ref href="{stream_url(index, providers)}" />')
            output.write("</entry>\n")
        output.write("</asx>\n")


def write_wpl(path: Path, streams: int, providers: int) -> None:
    """Write a deterministic Windows Media Playlist fixture."""
    with path.open("w", encoding="utf-8", newline="\n") as output:
        output.write('<?wpl version="1.0"?>\n<smil><head>')
        output.write(f'<meta name="ItemCount" content="{streams}"/>')
        output.write("</head><body><seq>\n")
        for index in range(streams):
            output.write(f'<media src="{stream_url(index, providers)}"/>\n')
        output.write("</seq></body></smil>\n")


def write_manifest(path: Path, streams: int, groups: int, providers: int) -> None:
    """Write fixture metadata used by smoke tests and benchmark reports."""
    path.write_text(
        json.dumps(
            {"streams": streams, "groups": groups, "providers": providers, "seed": 0},
            indent=2,
        )
        + "\n",
        encoding="utf-8",
    )


def main() -> None:
    """Parse command-line arguments and generate all requested fixtures."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--streams", type=int, default=10_000)
    parser.add_argument("--groups", type=int, default=100)
    parser.add_argument("--providers", type=int, default=4)
    args = parser.parse_args()
    if min(args.streams, args.groups, args.providers) < 1:
        parser.error("streams, groups and providers must all be positive")

    args.output.mkdir(parents=True, exist_ok=True)
    write_m3u(args.output / "playlist.m3u", args.streams, args.groups, args.providers)
    write_pls(args.output / "playlist.pls", args.streams, args.providers)
    write_xspf(args.output / "playlist.xspf", args.streams, args.providers)
    write_asx(args.output / "playlist.asx", args.streams, args.providers)
    write_wpl(args.output / "playlist.wpl", args.streams, args.providers)
    write_manifest(args.output / "manifest.json", args.streams, args.groups, args.providers)


if __name__ == "__main__":
    main()
