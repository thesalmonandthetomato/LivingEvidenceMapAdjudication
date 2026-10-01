# Workflow 02 adjudication contract

## Purpose

Workflow 02 enriches missing bibliographic metadata from Europe PMC and Scopus. Provider values that fail deterministic identity guards are quarantined for human review. Shiny records decisions only; it does not edit canonical records directly.

## Case identity

A Workflow 02 review case is uniquely identified by:

- `review_case_id`
- `record_id`
- `provider`
- `field`
- `reason`
- `queue_sha256`

The queue is immutable and SHA-256 validated before review.

## Allowed decisions

### Field-level identity-guard conflicts

Applies to:

- `title_mismatch`
- `title_guard_failed`

Allowed decisions:

- `accept_provider_field`
  - The reviewer confirms that the provider response refers to the same bibliographic record.
  - Downstream effect: the quarantined provider value may be used only for the named missing field.
  - Existing non-missing canonical values must never be overwritten.
  - The canonical DOI is not changed.

- `reject_provider_field`
  - The reviewer rejects the provider value for this record/field.
  - Downstream effect: no field value is applied.

- `uncertain`
  - No scientific change is permitted.
  - The case remains unresolved and cannot be considered complete for an automatic W02 resume.

### Returned DOI mismatch

Applies to:

- `returned_doi_mismatch`

Allowed decisions:

- `reject_provider_match`
  - The mismatching provider response is not used.

- `uncertain`
  - The case remains unresolved.

There is deliberately no automatic `accept` action for a returned DOI mismatch. Accepting metadata from a different DOI would require a separate bibliographic identity repair with explicit provenance, rather than ordinary Workflow 02 enrichment.

## Decision schema

Schema:

`living-evidence-map-workflow02-human-decision-v1`

Required fields:

- `review_case_id`
- `record_id`
- `provider`
- `field`
- `reason`
- `decision`
- `reviewer`
- `resolved_at_utc`
- `queue_sha256`

Optional:

- `note`
- `supersedes_decision_id`

## Completion

A W02 batch is complete only when every case has a non-`uncertain` active decision.

## Downstream adapter requirements

The future W02 resume adapter must:

1. verify queue SHA-256;
2. require every decision case ID to belong to the frozen queue;
3. resolve only the latest active decision per case;
4. refuse completion if any case is missing or `uncertain`;
5. apply only accepted field-level values to fields that remain missing;
6. never overwrite existing canonical title, abstract, author keywords or DOI;
7. never treat a returned DOI mismatch as an ordinary enrichment acceptance;
8. emit a deterministic repair patch plus manifest;
9. validate exact replay before W02 continues.
