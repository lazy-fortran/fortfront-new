#!/usr/bin/env python3
"""Independent expected-record and mutation oracle for the lexical adapter."""

from __future__ import annotations

import json


def expected_records() -> list[dict[str, object]]:
    return [
        {"symbol": "CALLER-UPPER", "start_byte": 0, "end_byte": 2,
         "scalar_count": 2, "fact": "UPPER", "status": "match"},
        {"symbol": "CALLER-GREEK", "start_byte": 2, "end_byte": 4,
         "scalar_count": 1, "fact": "GREEK", "status": "match"},
    ]


def expected_status_map() -> dict[str, str]:
    return {
        "match": "match",
        "no-match": "no-match",
        "unsupported": "unsupported",
        "ambiguous": "ambiguous",
        "invalid-utf8": "malformed",
    }


def mutation_expectations() -> list[dict[str, object]]:
    return [
        {"mutation": "unknown-scan-status", "status": "malformed"},
        {"mutation": "invalid-utf8-scan-span", "status": "malformed"},
        {"mutation": "too-small-output", "status": "capacity"},
        {"mutation": "negative-scan-count", "status": "malformed"},
    ]


def main() -> None:
    records = expected_records()
    assert records[0]["end_byte"] == 2 and records[1]["start_byte"] == 2
    assert records[1]["end_byte"] == 4  # α occupies two UTF-8 bytes.
    assert expected_status_map()["no-match"] == "no-match"
    assert expected_status_map()["ambiguous"] == "ambiguous"
    assert expected_status_map()["invalid-utf8"] == "malformed"
    mutations = mutation_expectations()
    assert {item["status"] for item in mutations} == {"malformed", "capacity"}
    print(json.dumps({"records": records, "status_map": expected_status_map(),
                      "mutations": mutations}, sort_keys=True))


if __name__ == "__main__":
    main()
