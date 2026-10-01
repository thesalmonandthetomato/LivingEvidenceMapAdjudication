# LivingEvidenceMap Adjudication

Standalone deployment repository for the LivingEvidenceMap human adjudication Shiny app.

This repository contains only the deployable application and its tests. The scientific workflow remains in `thesalmonandthetomato/LivingEvidenceMap`.

## Posit Connect Cloud

Publish from:

- repository: `thesalmonandthetomato/LivingEvidenceMapAdjudication`
- branch: `main`
- primary file: `app.R`

Required secret variables:

- `LEM_ACCESS_KEY_SHA256`
- `LEM_GOOGLE_SERVICE_ACCOUNT_JSON`
- `LEM_GOOGLE_SHEET_ID`
- `LEM_STORAGE_BACKEND=google_sheets`

Optional:

- `LEM_REVIEWER`

The public endpoint exposes only the access-key screen until authentication succeeds.

## Security

Do not commit access keys, Google service-account JSON, or local decision files.
