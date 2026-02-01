\# Codex Playbook — KSeF XML Service  

(ECC, ABAP 7.50, FA(3), KSeF API 2.0)



\## 0) Purpose

This playbook defines \*\*mandatory rules\*\* for Codex when working in this repository.



Goals:

\- keep implementation aligned with `XML\_SRV\_ARCHITECTURE.md`,

\- refactor legacy `zcl\_ksef\_data\_helper` safely,

\- produce changes that can be applied manually in ADT (copy/paste),

\- respect SAP ECC and ABAP 7.50 constraints.



Codex owns \*\*GitHub changes (code + docs)\*\*.  

Developer owns \*\*SAP GUI / ADT activation and testing\*\*.



---



\## 1) Documentation-first rule (STRICT)



Before coding, Codex MUST read documentation in this order:



1\. `docs/docs\_index.md`

2\. `docs/XML\_SRV\_ARCHITECTURE.md`  ← primary source for XML service

3\. `docs/KSEF\_ARCHITECTURE.md`     ← global integration rules

4\. `docs/XML\_FA3\_info/\*`           ← FA(3) / KSeF notes



Rules:

\- XML-related changes → update `XML\_SRV\_ARCHITECTURE.md`

\- Cross-layer changes → update `KSEF\_ARCHITECTURE.md`

\- New docs → update `docs\_index.md`



---



\## 2) Architectural constraints (NON-NEGOTIABLE)



\### 2.1 Layering

Codex must respect the target layering:



UI  

→ Orchestrator (`ZCL\_KSEF\_ORCH\_\*`)  

→ Foundation  

  • XML Service  

  • DB Repository  

→ Infrastructure



Strict prohibitions:

\- ❌ No SELECTs in XML builders/parsers/diff logic

\- ❌ No KSeF API calls inside XML Service

\- ❌ No UI logic inside Foundation



---



\## 3) XML Service responsibility boundaries



\### 3.1 XML Service (`ZCL\_KSEF\_FOUND\_XML\_SERVICE`)

Owns:

\- building FA(3) XML,

\- XML parsing for correction purposes,

\- diff logic for corrections,

\- XSD + business validation,

\- batch processing by `ksef\_id`.



Entry point:

\- batch-oriented, key = `ksef\_id`

\- partial success allowed (one invoice may fail, others succeed).



\### 3.2 DB Repository (`ZCL\_KSEF\_FOUND\_DB\_REPOSITORY`)

Owns:

\- all SELECTs,

\- batch reads,

\- mapping DB rows → raw data structures.



XML Service consumes repository output, never DB tables directly.



---



\## 4) Legacy refactoring rule (`zcl\_ksef\_data\_helper`)



Codex must treat `zcl\_ksef\_data\_helper` as \*\*legacy monolith\*\*.



Migration strategy:

\- logic is \*\*moved out\*\*, not duplicated,

\- final state: helper is thin or deprecated,

\- no new logic added to legacy helper.



What moves out:

\- XML parsing (`parse\_\*\_from\_xml`)

\- diff logic (`items\_differs`, podmiot compares)

\- correction builders (`\*\_K`, `StanPrzed`)

\- XML tag extraction (`P\_15`, `KursWalutyZ`, etc.)



---



\## 5) Internal structure inside XML Service



Codex should structure logic into clear internal components

(local classes or separate Foundation classes):



\- \*\*Assembler\*\*

&nbsp; - repository data → domain invoice model

\- \*\*XML Reader\*\*

&nbsp; - XML → structures / simple tags

\- \*\*Diff Engine\*\*

&nbsp; - compare old vs new domain models

\- \*\*Correction Builder\*\*

&nbsp; - build `Podmiot\*K`, `FaWiersz`, `ZamowienieWiersz`

\- \*\*Renderer\*\*

&nbsp; - FA(3) structures → XML string



Rules:

\- parser ≠ diff ≠ builder (never mixed),

\- diff logic must be reusable and deterministic.



---



\## 6) Domain model rules



\- XML Service uses an \*\*internal invoice domain model\*\*.

\- UI / ALV / presentation structures must not leak into XML Service.

\- Domain model must be:

&nbsp; - neutral to SAP tables,

&nbsp; - stable for testing,

&nbsp; - independent from XML serialization order.



---



\## 7) ABAP development rules (ECC, 7.50)



\### 7.1 Syntax compatibility

Allowed:

\- ABAP 7.50 syntax only.

\- Classic Open SQL (no S/4-only features).

\- Standard internal tables, OO ABAP.



Forbidden:

\- ABAP features > 7.50,

\- CDS-only assumptions,

\- S/4-specific artifacts.



\### 7.2 Performance

\- Batch SELECTs only.

\- No `SELECT SINGLE` in loops.

\- Use hashed tables for lookup.

\- `FOR ALL ENTRIES` only with non-initial driver table.



\### 7.3 Internal tables

\- hashed table → key access,

\- sorted table → ordered comparison,

\- standard table → lists.



Always define keys for hashed/sorted tables.



---



\## 8) XML \& comparison rules



\- XML output must be deterministic.

\- Comparison must tolerate:

&nbsp; - formatting differences,

&nbsp; - initial vs empty values.

\- Amounts must not be compared as raw strings.

\- If normalization helpers exist → \*\*mandatory usage everywhere\*\*.



---



\## 9) Logging \& errors



\### Logging

\- BAL object: `KSEFOUT`

\- Subobject: `XML`

\- External ID: `KSEF\_ID`



Log:

\- start/end of XML generation,

\- validation steps,

\- all correction decisions.



\### Errors

\- XML Service throws `ZCX\_KSEF\_XML\_ERROR`

\- always keep `previous`

\- batch API returns per-invoice messages for UI.



---



\## 10) PR rules (VERY IMPORTANT)



\### 10.1 Size

\- Small PRs only.

\- One architectural decision per PR.



\### 10.2 PR description must include

\- What changed

\- Why (reference to architecture section)

\- Methods/classes affected

\- Manual ADT test checklist

\- Rollback note



\### 10.3 Copy/paste safety

Changes must be easy to apply manually:

\- avoid huge file rewrites,

\- prefer method-level changes,

\- list activation order if multiple objects.



---



\## 11) Manual test checklist template (MANDATORY)



Include in every PR:



\- \[ ] Copy changes into ADT

\- \[ ] Activate objects (list them)

\- \[ ] Generate XML for known `ksef\_id`

\- \[ ] Compare XML with baseline

\- \[ ] Validate FA(3) XSD

\- \[ ] Check BAL log `KSEFOUT/XML/<ksef\_id>`

\- \[ ] If correction touched: verify \*\_K and `StanPrzed`



---



\## 12) When in doubt

If requirements are unclear:

\- do NOT invent behavior,

\- implement scaffolding first (interfaces, skeletons),

\- document assumptions in PR,

\- prefer docs + structure over logic.



---



\## 13) Definition of Done

A PR is done when:

\- architecture remains consistent,

\- XML Service responsibilities are respected,

\- no new logic is added to legacy helper,

\- docs are updated,

\- changes are safe to apply in ADT.



