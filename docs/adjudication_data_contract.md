# Lean adjudication data contract

This document freezes the Phase 0 contract for the Shiny adjudication redesign.
It is intentionally prospective: it does not change the current app, Google
Sheets tabs, local decision storage or workflow dispatch behaviour.

## Scope

The app is designed for a small consultancy team. Each project should use its
own fork/copy and deployment rather than multi-project tenancy in one app.

The operational roles are:

- `administrator`
- `resolver`
- `reviewer`

The lean batch states are:

- `ready`
- `running`
- `awaiting_review`
- `ready_for_export`
- `export_pending`
- `locked`
- `failed`

## Minimum entities

### users

`user_id`, `email`, `display_name`, `role`, `active`

### review_batches

`batch_id`, `workflow`, `queue_sha256`, `status`,
`created_at_utc`, `locked_at_utc`

The integrity identity for a published batch is the pair
`batch_id + queue_sha256`.

### review_cases

`case_id`, `record_id`, `batch_id`, `case_index`, `case_json`

### assignments

`assignment_id`, `case_id`, `user_id`, `blind_group`, `status`

Initial assignment states are `assigned` and `complete`.

### decision_events

`decision_id`, `case_id`, `user_id`, `decision`, `version`,
`active`, `event_at_utc`, `queue_sha256`, `supersedes_decision_id`

Decision events are append-only in the redesigned architecture. A revision
creates a new version that points to the decision it supersedes. The original
event remains provenance.

## Integrity rules frozen at Phase 0

1. Batch queue hashes are SHA-256.
2. A locked batch cannot transition to another state in place.
3. Locked batches require a lock timestamp.
4. Revised decision events must identify the prior event they supersede.
5. The redesigned storage layer must eventually prevent more than one active
   decision per user/case assignment.
6. Export/consumption must remain bound to the exact batch ID and queue SHA.
7. This contract does not retrospectively rewrite existing completed batches.

## Deferred implementation

Phase 0 does **not**:

- replace the shared access key;
- change current Google Sheets layouts;
- change local overwrite-style storage;
- change any Shiny screen;
- create assignments;
- change workflow dispatch;
- change existing W01/W02/W04/W08 scientific decisions.

Those changes belong to later migration phases after this contract is tested.
