# Adjudication app performance improvements

**Status:** Deferred until the LivingEvidenceMap pipeline has been demonstrated to run successfully end to end.

**Scope:** Both Shiny application layouts (root `app.R`/`R/` and `multiuser_app/app.R`/`multiuser_app/R/`), as applicable to the actual deployed application. Review activities W01 (deduplication), W02 (enrichment), W04 (screening) and W08 (annotation), plus shared assignment and administration operations.

## Problem

Reviewers experience approximately one second of delay when advancing between records, and occasionally much longer pauses when the Google Sheets backend retries requests (including a displayed retry after one minute). Even after removing an unnecessary routine post-append verification read in W01, the synchronous Google Sheets write and related state refreshes remain potential bottlenecks. The exact latency contributions have not yet been measured.

The priority is faster record-to-record interaction **without** sacrificing reliable writes, human decision provenance, multi-reviewer coordination, or review submission integrity.

## Phase 1: Measure and optimise synchronous operation (lower risk)

1. **Instrument timings** for record render/advance, authority/conflict check, Sheets read, Sheets append, assignment refresh, progress counts and other backend operations. Track median, p95 and p99 latency by workflow and operation. Avoid logging credentials or sensitive record contents.
2. **Audit repeated I/O** on each save and navigation action in W01/W02/W04/W08. Eliminate redundant full-tab reads, repeated sheet metadata/auth requests and unnecessary UI rebuilding. Retain one authoritative persistent write per decision.
3. **Use narrowly scoped caching** for immutable queue/record content and read-only metadata. Invalidate mutable caches on writes, queue changes, assignment changes and cross-session updates. Do not use stale cached decisions as the sole basis for cross-reviewer conflict/ownership checks.
4. **Optimise display updates:** preload the next record where possible; update local position/progress counters immediately when consistent with confirmed state; avoid rerendering unaffected panels.
5. **Check backend quota and contention:** characterise rate limiting, concurrent users, full-sheet read costs and Google Sheets retry behaviour; evaluate bounded, safe request handling without blind resubmission of ambiguous writes.
6. **Preserve workflow semantics:** W01 duplicate decisions and supersession; W02 enrichment; W04 double-blind independence, assignments and adjudication; W08 coding and geographic review; admin and queue lifecycle controls.

**Phase 1 gate:** Regression tests pass and measured user-perceived latency improves without a rise in save errors, conflicting decisions, missing entries or quota exhaustion. A few hundred milliseconds per typical record transition is a working target, not a promise.

## Phase 2: Asynchronous persistence (only if Phase 1 is insufficient)

Design a durable per-user pending-decision mechanism so navigation can proceed before the network write is complete, with explicit **pending / saved / failed / uncertain** states. Do not mark a record definitively complete until the backend confirms persistence.

Required controls:

- Durable pending queue or equivalent recoverable mechanism across transient server/reconnection failures; never rely solely on volatile browser memory.
- Idempotent decision IDs and reconciliation for ambiguous write outcomes; do not blindly retry an append that may already have succeeded.
- Ordered processing and clear supersession semantics if a reviewer revises a pending decision.
- Authoritative reviewer-conflict and assignment checks; W04 must retain double-blind separation.
- Visible unsaved count, actionable errors, recovery workflow and no silent loss or misattribution of decisions.
- **Block final handoff/export and backend queue reset** while writes remain pending, failed or uncertain.
- Backpressure and graceful handling of Google Sheets throttling, including multiple simultaneous reviewers.

Pilot on W01 after comprehensive concurrency/failure tests, then extend to W02, W04 and W08 only after the pilot is shown to preserve integrity.

**Phase 2 target:** Near-instant record navigation where feasible (aim under 200 ms on representative conditions), independent of background save latency. Confirm through measurements rather than assumptions.

## Validation and rollout

- Unit and integration tests for write acknowledgment, ambiguous timeout, retries, duplicate IDs, conflicting reviewers, reconnect, application restart, edits/supersession, batch changes and simultaneous users.
- Explicit tests of W04 double-blind independence, W01 review handoff, and W02/W08 review-specific data integrity.
- Verify consistency between root and `multiuser_app` implementations, and confirm which tree Connect Cloud actually deploys before rollout.
- Test against a **separate test sheet and queue**; do not reset production queues or overwrite existing decisions.
- Compare before/after timing and backend API request counts; release incrementally with rollback provisions.
- Preserve all decision audit trails and provenance.

## Dependencies and scheduling

**Do not implement during pipeline functional validation.** Resume this app-development task only when the complete pipeline, including Shiny adjudication handoff and downstream workflows, has passed the agreed end-to-end tests. No performance optimisation should be used to conceal or work around a failing pipeline workflow.
