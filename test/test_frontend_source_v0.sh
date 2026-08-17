#!/usr/bin/env bash
set -eu

root=$(cd "$(dirname "$0")/.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

positive="$tmp/positive.f90"
negative="$tmp/negative.f90"
printf '%s\n' 'program p' 'end program p' >"$positive"
printf '%s\n' 'program p' 'end program q' >"$negative"
fo exec fortfront-source-v0 "$positive" "$tmp/positive.sx"
fo exec fortfront-source-v0 "$positive" "$tmp/positive-repeat.sx"
fo exec fortfront-source-v0 "$negative" "$tmp/negative.sx"

test "$(cat "$tmp/positive.sx")" = \
    '(frontend-result (status accepted) (root-kind program) (diagnostic-count 0))'
test "$(cat "$tmp/negative.sx")" = \
    "(frontend-result (status rejected) (root-kind none) (diagnostic-count 1) (diagnostics (diagnostic (status rejected) (severity error) (message invalid-program) (span (file $negative) (start-byte 0) (end-byte 24) (source-hash l3-raw-program-v0)))))"
cmp "$tmp/positive.sx" "$tmp/positive-repeat.sx"
if fo exec fortfront-source-v0 "$positive" >/dev/null 2>&1; then
    exit 1
fi
printf '%s\n' 'frontend source-v0 process checks: ok'
