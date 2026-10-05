# Production multi-user adjudication cutover

Date: 2026-10-05

## Production implementation

The tested multi-user adjudication implementation is now deployed from the repository root on `main`.

Posit Connect Cloud should publish from:

- repository: `thesalmonandthetomato/LivingEvidenceMapAdjudication`
- branch: `main`
- primary file: `app.R`

The root production app and support modules are byte-identical to the tested `multiuser_app/` reference at the cutover commit.

`multiuser_app/` remains in the repository as the frozen development/reference copy for the cutover period. It is not the production deployment target.

## Cutover commits

- production promotion: `8827996f07c4c58a6cc573a54e76de62c919a155`
- promoted test dependency fix: `6ddd9683d9f14671360097ad3a440c0e29b872f3`
- generated production manifest: `dce7ac55a75679f61939f87766275cc36de99361`

Validation run:

- `37285701584` — full production parse, regression suite and root Connect Cloud manifest generation passed.

## Rollback

The complete pre-cutover production repository state is preserved at:

- branch: `production-legacy-adjudication-backup-20261005`
- SHA: `4ff7297c1b77514451fa6a3ba69d693d28e12de8`

Rollback procedure:

1. repoint/reset `main` to the rollback SHA or deploy the rollback branch;
2. regenerate/publish the corresponding root `manifest.json` if required by Connect Cloud;
3. verify the legacy login/dashboard opens before resuming use.

Keep this rollback branch until at least one real post-cutover pipeline cycle has passed.

## State and provenance

The production application continues to use the existing external Google Sheets backend and existing queue/decision schemas. The cutover does not intentionally clear, rewrite or migrate completed decisions, assignments, user rows or workflow queues.

Required production environment variables remain unchanged:

- `LEM_ACCESS_KEY_SHA256`
- `LEM_GOOGLE_SERVICE_ACCOUNT_JSON`
- `LEM_GOOGLE_SHEET_ID`
- `LEM_STORAGE_BACKEND=google_sheets`

Existing optional/user-specific authentication and GitHub dispatch variables remain supported by the promoted multi-user implementation.

## CI

`.github/workflows/validate.yml` now treats the repository root as the sole production application:

- parses `app.R` and every root `R/*.R` file;
- runs the full promoted multi-user regression suite from root `tests/`;
- generates only the root production Connect Cloud manifest; and
- commits `manifest.json` if it changes.

The workflow does not dispatch scientific workflows or call paid APIs/LLMs.

## Post-cutover smoke test

Before treating the migration as operationally complete, verify in the live Posit deployment:

1. administrator login succeeds;
2. reviewer login succeeds;
3. role-restricted controls differ correctly;
4. W01, W02, W04 and W08 cards render;
5. W04 validation, reviewer consistency and conflict-resolution routes render where applicable;
6. progress/KPI values are readable from the existing Sheets backend;
7. assigning and removing a harmless test assignment works;
8. a non-destructive test decision round-trip works only in a test queue;
9. no completed production decisions or queues change unexpectedly.

Do not exercise production workflow-resume buttons merely to smoke-test the interface.
