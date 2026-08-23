#!/usr/bin/env python3
"""Independent whole-stream model for lexical grammar session transactions."""

from dataclasses import dataclass, field

ACCEPTED, REJECTED, AMBIGUOUS, UNRESOLVED = "accepted", "rejected", "ambiguous", "unresolved"
MALFORMED, CAPACITY = "malformed", "capacity"
NO_MATCH, UNSUPPORTED = "no-match", "unsupported"
TOKEN_CAPACITY = "token-capacity"
TOKEN_AMBIGUOUS = "token-ambiguous"


@dataclass
class Stream:
    alternatives: tuple[tuple[str, ...], ...]
    tokens: list[dict[str, object]]
    frontier_limit: int = 8
    token_limit: int = 8
    unresolved: bool = False
    cursor: int = 0
    symbols: list[str] = field(default_factory=list)

    def __post_init__(self) -> None:
        if not self.alternatives or any(not rule for rule in self.alternatives):
            raise ValueError(MALFORMED)

    def frontier(self, symbols: list[str]) -> tuple[str, int]:
        if self.unresolved:
            return UNRESOLVED, 1
        matches = sum(tuple(symbols) == rule for rule in self.alternatives)
        if matches == 0:
            return REJECTED, 0
        return (AMBIGUOUS if matches > 1 else ACCEPTED), matches

    def consume(self) -> tuple[str, list[dict[str, object]], int]:
        trial_cursor, trial_symbols = self.cursor, list(self.symbols)
        selected: list[dict[str, object]] = []
        while trial_cursor < len(self.tokens):
            token = self.tokens[trial_cursor]
            kind = token.get("status")
            if kind != "match":
                return {"no-match": NO_MATCH, "unsupported": UNSUPPORTED,
                        "ambiguous": TOKEN_AMBIGUOUS}.get(kind, MALFORMED), [], 0
            symbol = token.get("symbol")
            if not isinstance(symbol, str) or not symbol:
                return MALFORMED, [], 0
            outcome, count = self.frontier([*trial_symbols, symbol])
            if count > self.frontier_limit:
                return CAPACITY, [], 0
            if len(selected) == self.token_limit:
                return TOKEN_CAPACITY, [], 0
            selected.append(token)
            trial_symbols.append(symbol)
            trial_cursor += 1
        outcome, count = self.frontier(trial_symbols)
        if count > self.frontier_limit:
            return CAPACITY, [], 0
        self.cursor, self.symbols = trial_cursor, trial_symbols
        return outcome, selected, count


def require(value: bool, message: str) -> None:
    if not value:
        raise AssertionError(message)


def main() -> None:
    good = Stream((("a", "b"),), [{"symbol": "a", "status": "match"},
                                   {"symbol": "b", "status": "match"}])
    status, selected, frontier_count = good.consume()
    require((status, [x["symbol"] for x in selected], frontier_count) == (ACCEPTED, ["a", "b"], 1),
            "ordered success or source records changed")

    retry = Stream((("a", "b"),), good.tokens, token_limit=1)
    status, _, _ = retry.consume()
    require(status == TOKEN_CAPACITY and retry.cursor == 0 and retry.symbols == [], "token rollback failed")
    retry.token_limit = 2
    require(retry.consume()[0] == ACCEPTED and retry.cursor == 2, "token retry failed")

    frontier_retry = Stream((("a",), ("a",)), [{"symbol": "a", "status": "match"}], frontier_limit=1)
    require(frontier_retry.consume()[0] == CAPACITY and frontier_retry.cursor == 0, "frontier rollback failed")
    frontier_retry.frontier_limit = 2
    require(frontier_retry.consume()[0] == AMBIGUOUS, "frontier retry failed")

    for lexical, expected in (("no-match", NO_MATCH), ("unsupported", UNSUPPORTED),
                              ("ambiguous", TOKEN_AMBIGUOUS), ("bad", MALFORMED)):
        controlled = Stream((("a",),), [{"symbol": "a", "status": lexical}])
        require(controlled.consume()[0] == expected and controlled.cursor == 0, "lexical negative control failed")
    require(Stream((("a",),), [{"symbol": "x", "status": "match"}]).consume()[0] == REJECTED,
            "final rejection changed")
    require(Stream((("a",), ("a",)), [{"symbol": "a", "status": "match"}]).consume()[0] == AMBIGUOUS,
            "final ambiguity changed")
    require(Stream((("a",),), [{"symbol": "a", "status": "match"}], unresolved=True).consume()[0] == UNRESOLVED,
            "unresolved grammar changed")
    try:
        Stream((), [])
    except ValueError as error:
        require(str(error) == MALFORMED, "malformed session control changed")
    else:
        raise AssertionError("malformed session was accepted")
    print("lexical grammar stream independent oracle: ok")


if __name__ == "__main__":
    main()
