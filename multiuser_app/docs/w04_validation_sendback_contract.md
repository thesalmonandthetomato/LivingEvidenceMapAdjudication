# Workflow 04 validation send-back contract

## Purpose

A completed Workflow 04 validation set serves two purposes:

1. estimate agreement between the frozen model screening output and independent human screening; and
2. return the human decisions for the sampled records so the Workflow 04 backend can later apply them as authoritative manual overrides.

This document defines only the Shiny-side hand-off. It does not implement the receiving logic in `LivingEvidenceMap`.

## Shiny responsibilities

Before dispatch, the multi-user app must fail closed unless all of the following are true:

- the active manual-screening mode is `validation_set`, not reviewer-consistency;
- the batch ID is present and the queue SHA is a valid SHA-256;
- every frozen queue case has exactly one active assignment;
- every frozen queue case has exactly one active human decision;
- the decision belongs to the reviewer assigned to that case;
- the decision record ID matches the record ID bound to that review case;
- every returned decision carries the exact active queue SHA;
- every decision is one of `retain`, `exclude`, or `uncertain`.

Reviewer-consistency batches, mixed assignment modes, incomplete batches, duplicate assignments, duplicate decisions, record mismatches, reviewer mismatches and queue-SHA mismatches must never be dispatched as completed validation sets.

## Dispatch

Only a user with `control_workflows` may send the validation set back to GitHub.

The app dispatches:

- workflow: `workflow_04_finalize_human_validation.yml`
- ref: `workflow01-final-architecture`
- input `batch_id`
- input `queue_sha256`

The individual validation decisions remain in the configured Google Sheet decision log. The receiving Workflow 04 job is expected to retrieve the exact decision set identified by the batch/queue contract and validate it again before use.

## Backend responsibility not implemented here

The `LivingEvidenceMap` backend must still be updated separately to:

- validate the same batch/queue/record invariants;
- materialise the returned human decisions as a durable manual-override layer;
- preserve the original model decisions for agreement calculations and provenance;
- apply human decisions as final decisions for sampled records;
- calculate/store validation statistics;
- finalise and publish the post-override Workflow 04 state.

No backend change is made by this Shiny-side implementation.
