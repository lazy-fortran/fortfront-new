#!/usr/bin/env python3
"""Independent oracle for ordered lexical projection into a small grammar."""

from __future__ import annotations


def run(tokens: list[dict[str, object]], grammar: list[str]) -> tuple[list[str], int]:
    if not grammar:
        raise AssertionError("grammar has no start candidate")
    forwarded: list[str] = []
    cursor = 0
    for token in tokens:
        assert cursor < len(tokens)
        assert token is tokens[cursor]
        if token["status"] != "match":
            return forwarded, cursor
        forwarded.append(str(token["symbol"]))
        cursor += 1
    return forwarded, cursor


def main() -> None:
    tokens = [
        {"symbol": "first", "status": "match"},
        {"symbol": "unsupported", "status": "unsupported"},
        {"symbol": "second", "status": "match"},
    ]
    forwarded, cursor = run(tokens, ["first", "second"])
    assert forwarded == ["first"]
    assert cursor == 1  # unsupported is rejected transactionally and remains next.

    for bad in (
        {"symbol": "ambiguous", "status": "ambiguous"},
        {"symbol": "broken", "status": "malformed-span"},
        {"symbol": "broken", "status": "malformed-provenance"},
    ):
        forwarded, cursor = run([bad], ["broken"])
        assert forwarded == [] and cursor == 0

    try:
        run([{"symbol": "cursor", "status": "match"}], [])
    except AssertionError:
        pass
    else:
        raise AssertionError("empty cursor negative control did not fail")
    print("lexical grammar session independent oracle: ok")


if __name__ == "__main__":
    main()
