# Multi-user adjudication app: remaining integration TODOs

Do not treat the multi-user app as production-complete until these items are resolved.

## Workflow 04 reviewer conflict resolution

The **Reviewer conflict resolution** dashboard block is currently only a placeholder UI.

Before finalisation:
- implement the actual conflict case view;
- allow both `administrator` and `reviewer` roles to make the final scientific Include/Exclude adjudication;
- persist the final adjudicated decision with authenticated `user_id`, queue SHA, timestamp and supersession/audit provenance;
- preserve the independent reviewer/model decisions used to generate the conflict;
- do not use the adjudicated result when calculating independent agreement statistics;
- once the batch is complete, require administrator-only workflow continuation/export.

## Workflow 04 validation / manual screening integration

The **Manual screening / validation** block must be checked end-to-end against the GitHub workflow before the app is considered complete.

Before finalisation:
- verify that the Shiny queue corresponds exactly to the GitHub-generated frozen validation/manual-screening batch;
- verify queue SHA/batch ID preservation through adjudication;
- verify that saved decisions are returned to the correct GitHub Workflow 04 finalisation/resume path;
- verify exact coverage and fail-closed behaviour before GitHub consumes the batch;
- verify acknowledgement/locking after GitHub consumption;
- confirm that model outputs remain hidden during blinded validation until the required human reviews are complete;
- test the complete Shiny -> storage -> GitHub -> acknowledgement cycle on a non-production batch.

These checks should be completed after the remaining multi-user decision-event, assignment, blind-review and conflict-resolution architecture is in place.
