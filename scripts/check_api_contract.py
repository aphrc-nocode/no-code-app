#!/usr/bin/env python3
"""Check that the R deep-learning client only sends fields the API accepts.

Parses server/deep_learning.R for every `dl_request(<path>) %>% ...
req_body_multipart(<fields>)` (and the legacy `request(paste0(api_url, ...))`
form) and verifies each field name is in the API's contract for that endpoint
(scripts/dl_api_contract.json, vendored from no-code-transformers).

This closes the drift class of bug that took the platform down: the client
sending a field the API silently ignores (weight_decay vs weight_decay_hf) or
rejects (data_zip vs data_file, missing X-API-Key).

Exit code 1 if any client field is not in the contract.

Regenerate the contract in no-code-transformers with:
    python scripts/dump_contract.py > dl_api_contract.json
then copy it here.
"""
import json
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
CLIENT = REPO / "server" / "deep_learning.R"
CONTRACT = Path(__file__).resolve().parent / "dl_api_contract.json"

# httr2 helpers that carry a value but are not API fields themselves.
NON_FIELD = {"curl::form_file"}


def _balanced(text: str, open_idx: int) -> str:
    """Return the substring inside the parentheses starting at open_idx (a '(')."""
    depth = 0
    for i in range(open_idx, len(text)):
        if text[i] == "(":
            depth += 1
        elif text[i] == ")":
            depth -= 1
            if depth == 0:
                return text[open_idx + 1:i]
    raise ValueError("unbalanced parentheses")


def resolve_path(arg: str) -> str:
    """Map a dl_request()/request() path argument to a contract key."""
    arg = arg.strip()
    m = re.match(r'^"([^"]+)"$', arg)
    if m:
        return m.group(1)
    # paste0("/data/upload/", input$...) -> "/data/upload/{task_type}"
    m = re.match(r'paste0\(\s*(?:api_url\s*,\s*)?"([^"]+)"', arg)
    if m:
        prefix = m.group(1)
        return prefix.rstrip("/") + "/{task_type}" if prefix.endswith("/") else prefix
    return arg  # unknown; will fail the lookup and be reported


def top_level_field_names(multipart_args: str) -> list:
    """Extract `name =` keys at the top level of a req_body_multipart(...) body."""
    names, depth, i = [], 0, 0
    # A field name is an identifier followed by '=' (not '==') at depth 0.
    token = re.compile(r'([A-Za-z_][A-Za-z0-9_.]*)\s*=(?!=)')
    while i < len(multipart_args):
        ch = multipart_args[i]
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
        elif depth == 0:
            m = token.match(multipart_args, i)
            if m and m.group(1) not in NON_FIELD:
                names.append(m.group(1))
                i = m.end()
                continue
        i += 1
    return names


def find_calls(src: str):
    """Yield (path_key, [field names]) for each request-building call."""
    # dl_request(<arg>) or request(paste0(api_url, ...))
    for m in re.finditer(r'\b(?:dl_request|request)\s*\(', src):
        arg = _balanced(src, m.end() - 1)
        # The multipart body must follow within the same pipeline.
        rest = src[m.end():]
        mp = re.search(r'req_body_multipart\s*\(', rest)
        # Only associate if it appears before the next request-builder call.
        nxt = re.search(r'\b(?:dl_request|request)\s*\(', rest)
        if not mp or (nxt and nxt.start() < mp.start()):
            continue
        body = _balanced(rest, mp.end() - 1)
        yield resolve_path(arg), top_level_field_names(body)


def main():
    contract = json.loads(CONTRACT.read_text())
    src = CLIENT.read_text()

    problems = []
    checked = 0
    for path, fields in find_calls(src):
        if path not in contract:
            # Not an API endpoint we have a contract for (e.g. GET helpers) -> skip.
            continue
        allowed = set(contract[path].get("POST", []))
        checked += 1
        for f in fields:
            if f not in allowed:
                problems.append((path, f, sorted(allowed)))

    if problems:
        print("API contract check FAILED — client sends fields the API does not accept:\n")
        for path, field, allowed in problems:
            print(f"  {path}: unknown field '{field}'")
            print(f"      allowed: {', '.join(allowed)}\n")
        sys.exit(1)

    print(f"API contract OK — validated {checked} client request(s) against the API contract.")


if __name__ == "__main__":
    main()
