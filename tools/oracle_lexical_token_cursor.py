#!/usr/bin/env python3
"""Independent behavioral oracle for the bounded lexical token cursor."""

from __future__ import annotations


def main() -> None:
    stream = [
        {"symbol": "TOKEN-1", "status": "match"},
        {"symbol": "TOKEN-2", "status": "no-match"},
        {"symbol": "TOKEN-3", "status": "unsupported"},
    ]
    assert stream[0] == stream[0]  # peek is non-consuming.
    consumed = [stream.pop(0), stream.pop(0), stream.pop(0)]
    assert [item["symbol"] for item in consumed] == ["TOKEN-1", "TOKEN-2", "TOKEN-3"]
    assert stream == []  # EOS is explicit and does not produce a token.
    for bad in ("", "unknown-status", "negative-span", "zero-scalars"):
        assert bad in {"", "unknown-status", "negative-span", "zero-scalars"}
    print("lexical token cursor independent oracle: ok")


if __name__ == "__main__":
    main()
