#!/usr/bin/env python3
"""Independent small grammar-session model for the lexical adapter."""

from __future__ import annotations

from dataclasses import dataclass, field


ACCEPTED = "accepted"
REJECTED = "rejected"
AMBIGUOUS = "ambiguous"
UNRESOLVED = "unresolved"
MALFORMED = "malformed"
CAPACITY = "capacity"
NO_MATCH = "no-match"
UNSUPPORTED = "unsupported"
TOKEN_AMBIGUOUS = "token-ambiguous"
END = "end-of-stream"


@dataclass
class Model:
    alternatives: list[tuple[str, ...]]
    unresolved: bool = False
    capacity: int = 16
    tokens: list[dict[str, str]] = field(default_factory=list)
    cursor: int = 0
    symbols: list[str] = field(default_factory=list)

    def __post_init__(self) -> None:
        if not self.alternatives:
            raise ValueError(MALFORMED)
        if any(not alternative for alternative in self.alternatives):
            raise ValueError(MALFORMED)

    def evaluate_symbols(self, symbols: list[str]) -> str:
        if self.unresolved:
            return UNRESOLVED
        matches = [alternative for alternative in self.alternatives
                   if tuple(symbols) == alternative]
        if len(matches) > self.capacity:
            return CAPACITY
        if len(matches) == 0:
            return REJECTED
        return ACCEPTED if len(matches) == 1 else AMBIGUOUS

    def evaluate(self) -> str:
        return self.evaluate_symbols(self.symbols)

    def advance(self) -> str:
        if self.cursor == len(self.tokens):
            return END
        token = self.tokens[self.cursor]
        status = token["status"]
        if status != "match":
            return {"no-match": NO_MATCH, "unsupported": UNSUPPORTED,
                    "ambiguous": TOKEN_AMBIGUOUS}.get(status, MALFORMED)
        symbol = token.get("symbol", "")
        if not symbol:
            return MALFORMED
        candidate_symbols = [*self.symbols, symbol]
        outcome = self.evaluate_symbols(candidate_symbols)
        if outcome in (MALFORMED, CAPACITY):
            return outcome
        self.cursor += 1
        self.symbols = candidate_symbols
        return outcome

    def finalize(self) -> str:
        if self.cursor < len(self.tokens):
            status = self.tokens[self.cursor]["status"]
            if status != "match":
                return {"no-match": NO_MATCH, "unsupported": UNSUPPORTED,
                        "ambiguous": TOKEN_AMBIGUOUS}.get(status, MALFORMED)
            return MALFORMED
        return self.evaluate()


def require(condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


def main() -> None:
    session = Model([("first", "second")], tokens=[
        {"symbol": "first", "status": "match"},
        {"symbol": "unsupported", "status": "unsupported"},
        {"symbol": "second", "status": "match"},
    ])
    require(session.advance() == REJECTED, "prefix outcome differs")
    require(session.advance() == UNSUPPORTED, "unsupported was forwarded")
    require(session.cursor == 1, "unsupported consumed the cursor")
    require(session.finalize() == UNSUPPORTED, "lexical failure finalized as accepted")

    capacity_retry = Model([("x",), ("x",)], capacity=1,
                           tokens=[{"symbol": "x", "status": "match"}])
    require(capacity_retry.advance() == CAPACITY, "capacity outcome differs")
    require(capacity_retry.cursor == 0 and capacity_retry.symbols == [],
            "capacity consumed the token")
    capacity_retry.capacity = 2
    require(capacity_retry.advance() == AMBIGUOUS, "capacity retry outcome differs")
    require(capacity_retry.cursor == 1 and capacity_retry.symbols == ["x"],
            "capacity retry did not commit the token")

    pending = Model([("x",)], tokens=[{"symbol": "x", "status": "match"}])
    require(pending.finalize() == MALFORMED, "matched finalization was accepted")
    require(pending.cursor == 0 and pending.symbols == [],
            "matched finalization consumed the token")
    require(pending.advance() == ACCEPTED and pending.cursor == 1,
            "matched token was not available after finalization")

    malformed_push = Model([("x",)], tokens=[{"symbol": "", "status": "match"}])
    require(malformed_push.advance() == MALFORMED, "malformed grammar push differs")
    require(malformed_push.cursor == 0 and malformed_push.symbols == [],
            "malformed grammar push consumed the token")

    for status, expected in (("no-match", NO_MATCH), ("unsupported", UNSUPPORTED),
                             ("ambiguous", TOKEN_AMBIGUOUS), ("malformed", MALFORMED)):
        isolated = Model([("x",)], tokens=[{"symbol": "x", "status": status}])
        require(isolated.advance() == expected, f"{status} status changed")
        require(isolated.cursor == 0, f"{status} consumed the cursor")
        require(isolated.finalize() == expected, f"{status} finalized as accepted")

    require(Model([("x",)], tokens=[{"symbol": "x", "status": "match"}]).advance()
            == ACCEPTED, "accepted outcome differs")
    require(Model([("x",)], tokens=[{"symbol": "y", "status": "match"}]).advance()
            == REJECTED, "rejected outcome differs")
    require(Model([("x",), ("x",)], tokens=[{"symbol": "x", "status": "match"}]).advance()
            == AMBIGUOUS, "ambiguous outcome differs")
    require(Model([("x",)], unresolved=True,
                  tokens=[{"symbol": "x", "status": "match"}]).advance()
            == UNRESOLVED, "unresolved outcome differs")
    try:
        Model([], tokens=[])
    except ValueError as error:
        require(str(error) == MALFORMED, "empty grammar negative control changed")
    else:
        raise AssertionError("empty grammar was accepted")
    print("lexical grammar session independent oracle: ok")


if __name__ == "__main__":
    main()
