\# Codex Instructions (Entry Point)



Start here:

1\) Read `docs/codex\_playbook.md` (MUST follow).

2\) Use `docs/docs\_index.md` to find relevant docs.

3\) For XML Service changes: read `docs/XML\_SRV\_ARCHITECTURE.md`.



Rules:

\- Small PRs only.

\- ECC / ABAP 7.50 only.

\- KSeF API 2.0, FA(3).

\- No new logic in legacy `zcl\_ksef\_data\_helper`.

## Decision log policy
- Codex MUST read `docs/decisions.md`.
- Codex MUST NOT commit changes to `docs/decisions.md` directly.
- If a PR introduces a new architectural/design decision, Codex MUST include a
  "Suggested decisions.md update" section in the PR description.


