# Multi-user adjudication app: remaining integration TODOs

## Workflow 04 reviewer consistency and conflict resolution

Implemented in `main/multiuser_app/`:

- independent blinded reviewer-consistency assignments;
- model/human and human/human consistency analyses;
- saved analysis provenance;
- generated conflict sets;
- explicit conflict assignments;
- conflict adjudication by permitted users;
- independent decisions preserved for agreement statistics.

## Workflow 04 validation-set send-back

Implemented and regression-tested on the Shiny side:

- validation-set assignments are kept distinct from reviewer-consistency assignments;
- exact queue coverage is required before send-back;
- every case must have exactly one active assignment and one active decision;
- assigned reviewer, record ID and queue SHA must match;
- invalid/incomplete/ambiguous batches fail closed;
- only administrators with `control_workflows` can dispatch;
- the dispatch contract is `batch_id` + `queue_sha256` to
  `workflow_04_finalize_human_validation.yml` on
  `workflow01-final-architecture`.

See `docs/w04_validation_sendback_contract.md`.

## Still outstanding outside this repository

The receiving `LivingEvidenceMap` Workflow 04 backend must be updated separately to:

- retrieve and revalidate the completed validation-set decisions;
- ingest them as an authoritative manual-override layer;
- preserve original model decisions;
- calculate/store validation statistics;
- apply human overrides before final Workflow 04 publication/Zenodo output;
- acknowledge/lock the consumed Shiny batch.

Do not treat that backend work as complete merely because the Shiny dispatch succeeds.
