\# Architectural \& Design Decisions



\## 2026-02-01 — XML Service introduced (skeleton)

Decision:

\- XML Service introduced as a separate Foundation component.

\- Initial implementation is a skeleton (no behavior change).



Reason:

\- Prepare safe refactoring of legacy XML logic.

\- Align implementation with XML\_SRV\_ARCHITECTURE.md.



Implications:

\- Legacy zcl\_ksef\_data\_helper remains unchanged.

\- Further PRs will gradually move logic into XML Service.



