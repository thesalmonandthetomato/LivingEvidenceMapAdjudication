# W04 Screening and Validation workspace

Status: Stage 1 COMPLETE; Stage 2 IMPLEMENTED (automated test PASS; live Google Sheets check PENDING).
Working branch: **main only**. Posit Connect deploys from `main/multiuser_app/`.
Last reviewed: 2026-10-18. Source: current `main` code, not a live Google Sheets export.

## Objective
One administrator-only W04 workspace with four dated, searchable tables: (1) subsets, (2) prompts, (3) model runs, (4) saved consistency analyses/conflicts. Keep all existing reviewer task cards and W01/W02/W08 controls operational. Prioritise read-only overview first. Do not write/recompute historical decisions or kappa on page load.

## Authoritative sources and joins

| Entity | Authoritative source | Key and linkage | Date / details |
|---|---|---|---|
| Human subset | Google Sheets `w04_subset_registry` | `review_id`, `subset_id` and `subset_json`; immutable membership | `created_at_utc`, `actual_size`, `parent_subset_id` |
| Subset display label | `w04_subset_labels` append-only labels | `review_id` + `subset_id`; newest label event wins | `changed_at_utc` |
| Prompt version | Restricted Zenodo draft, one `w04-<version>.json` per version | `version_id` + `sha256`; do not copy prompt contents to Sheets | `created_at_utc`, `author`, `notes`, `status` |
| Model run | Google Sheets `w04_model_batch_registry` | `run_id`, `subset_id`, `prompt_version_id`, `prompt_sha256` | `published_at_utc`, `total`, `retain`, `exclude`, `uncertain`. **Published date is not necessarily created/launch date.** |
| Human reviewer assignments | `assignments` (via `read_sheet_assignments`) | `workflow=04`, `task_type=manual_screening`, `batch_id=w04-human-<subset_id>`, `user_id`, `case_id` | Active assignment status must be applied; this is not evidence of a completed decision |
| Human screening decisions | W04 screening decision tab, `active_sheet_w04_decisions` | Filter by subset queue SHA; decision `review_case_id`, `reviewer` / reviewer ID | Progress requires verified decision-to-subset matching; no blind disclosure |
| Model uncertainty resolutions | `decisions_w04_resolution`, `active_sheet_w04_resolution_decisions` | Queue SHA and case identity | Independent from human–model disagreement adjudications |
| Saved comparisons | `w04_consistency_analyses` | `analysis_id`, `batch_id`, `queue_sha256`, `rater_ids_json` | `created_at_utc`, `raw_agreement`, `kappa`, `complete_n`, `conflict_n`, case IDs |
| Registered conflict set | `w04_conflict_sets` | `analysis_id` → comparison, `conflict_set_id` → assignments, `parent_batch_id`, `comparison_type`, `conflict_queue_sha256` | `created_at_utc`, `conflict_case_ids_json` |
| Conflict decisions | `decisions_w04_conflict`, `active_sheet_w04_conflict_decisions` | Filter by conflict queue SHA and case identity | Count resolved case IDs; do not merge decisions across historical queues |
| Reviewer directory | Existing user registry | `user_id` → display name | Resolve missing/retired users safely by ID |

## Findings / constraints
- Sources for subset, model and comparison dates already exist. Model-run *publication* timestamp exists; no separate launch timestamp is required by the batch reader. Display **Published** until a verified launch timestamp is available, not **Created**.
- Human-human versus human-model comparisons are distinguishable from `batch_id` and saved `rater_ids_json`; conflict sets also carry `comparison_type`.
- Prompt metadata is in restricted Zenodo, **not** presently a complete Google Sheets prompt registry. Fetch lazily / cache metadata once, never prompt contents per displayed table row.
- Existing W04 saved-analysis history is loaded on opening consistency checking, not continuously. A summary must not call full decision comparisons or individual model-result APIs on each render.
- A missing archived record/failed lookup must be displayed as unavailable, not guessed or silently treated as zero.
- All new code confined to `multiuser_app/`; no changes to root app or the LivingEvidenceMap pipeline.
- No separately deployable development branches; incremental commits on `main`, existing UI retained until proven safe.
- Review-scoped data must be filtered by review identity. Validate unique keys and avoid cross-run decision leakage.

## Implementation checklist
- [x] **Stage 1: Data inventory** (source-code review; real sheet contents not yet verified).
- [x] **Stage 2: Read-only overview implementation**. Event-loaded and session-cached tables for subsets, prompt versions, model runs, saved consistency analyses; dates visible. Automated tests passed. **Live Shiny check pending before moving W04 controls.**
- [ ] **Stage 3: Consolidate W04 navigation**. Retain existing controls until new workspace is validated. Move W04-only controls out of configure/admin.
- [ ] **Stage 4: Simplify comparisons and conflict assignment**. Idempotent reuse of existing sets; preserve assignments/decision history; user checkpoint.
- [ ] **Stage 5: Event-bound refresh/caching**. Avoid polling and unnecessary Zenodo traffic. Assess materialised Sheets summary only if needed.
- [ ] **Stage 6: End-to-end validation and cleanup**. Regression, permissions, live verification, remove obsolete W04 UI.

## Progress reporting and handover
For each stage: record commit SHA, precise changes, validation links and pass/fail, production checks still needed, and next action. Stop at planned live user check-ins after Stage 2 and Stage 4. Never claim an automated test verifies live Google Sheets contents without a live test.

## Completed commits and tests
- Stage 1 inventory: `73224f088050a6db7697b70b34b5fb40f6556d47`.
- Stage 2 read-only join helper: `ca621b91ca0d6f2c312b7e499001f87c36c5c29e` (`R/w04_workspace_overview.R`).
- Stage 2 isolated overview UI: `f3ad415a2a297f1764f6ba1ccf3a00f62259522f` (inside **Administration & assignments → W04 · Screening and validation overview**).
- Assignment status correction: `339471d19861da83f5985af7f3fcca25ad2d9989`; real status is `assigned`, not `active`.
- Automated regression: [GitHub Actions run 39108307539](https://github.com/thesalmonandthetomato/LivingEvidenceMapAdjudication/actions/runs/39108307539) **PASS** (R parser and pure registry joins, without Google Sheets/Zenodo access). Temporary workflow removed in `f0b3d89d99d37501a5d0e47742035ad165beaa01`.
- Stage 2 did not write Google Sheets, run a model or change any existing W04 control.

## Known limitations requiring live check
- **Refresh overview** explicitly reads Sheets registries and the restricted Zenodo prompt manifest. Metadata is cached in the Shiny session until refreshed. A live login is needed to assess latency, permissions and names.
- Progress is assembled from active decision events and assignment rows; verify subset-to-decision identities with real rows. Full live accuracy is not established by fixture tests.
- Model-run date is labelled **Published** rather than misleading **Created**; no reliable saved launch timestamp identified.
- The current read-only overview uses Shiny tables. Interactive search/filter and a dedicated unified navigation are pending Stage 3; existing controls remain available.

## Next action
**User checkpoint after Stage 2:** redeploy `main/multiuser_app/`, open **Administration & assignments → W04 · Screening and validation overview**, click **Refresh overview**, and confirm subset counts, reviewers, dates and model-run associations. If errors appear, capture their exact text. Do not start Stage 3 until this passes.

## 18 Oct: Administration loading safeguard
- Screenshot after Stage 2 showed task cards but no Administration menu yet. Source inspection found `output$assignment_progress` performs potentially slow historical W04 Sheets reads synchronously during initial rendering.
- Commit `22f91f1f644fb0ef811259ddedef4d1a63e17165` adds an immediately visible **Administration & assignments** entry button. Heavy assignment UI waits until that button is pressed; the existing accordion and W04 overview remain.
- [Regression 39114310791](https://github.com/thesalmonandthetomato/LivingEvidenceMapAdjudication/actions/runs/39114310791) **PASS** for R syntax, deferred rendering guard and unchanged controls. Temporary workflow removed in `9970ef03213af935030f93c14e1a31d616574ee7`.
- **Still unverified live:** entry button display timing, complete menu load after click, and W04 overview refresh. This does not establish that all historical Sheets reads are fast.

## 18 Oct: Compact and date-ordered overview
- User confirmed that the four overview tables load (4 subsets, 8 prompts, 8 runs, 3 comparisons), but found the tables too large and opaque.
- Commits `747ae4002ed90f9c8c66aa5c25f4d90b6232e77b` and `0641c3e6e2c6848cb0994a9d993a72ae3d047752` shorten visible names/dates, remove raw ID columns, compress column names and cell typography, and sort **each table newest-first using original UTC timestamp strings**. Original identifiers are preserved in source registries.
- [Automated regression 39114550448](https://github.com/thesalmonandthetomato/LivingEvidenceMapAdjudication/actions/runs/39114550448) PASS; temporary workflow removed (`80a9a39077d727e008c908885501c56af858815b`).
- Live visual check of the compact layout still needed. A reviewer-progress correctness check against actual saved decision rows remains advisable before declaring Stage 2 fully signed off.

## 18 Oct: Single-sheet W04 overview and fast administrator entry (pending live verification)

**Architecture:** `multiuser_app/R/w04_work_overview_sheets.R` maintains append-only event rows in the review-specific Google Sheets tab `w04_work_overview`: `event_id`, `review_id`, `record_type`, `entity_id`, `event_at_utc`, `details_json`. All four main overview tables and a compact conflict-set table are reconstructed in memory from one summary-tab read. No prompt files or screening decision logs are retrieved on ordinary overview refresh.

**Write points:**
- Successful, verified subset creation: summary event (`w04_subset_storage.R`).
- Successful, verified prompt archive save: summary event (`w04_prompt_zenodo.R`).
- Verified consistency analysis and conflict-set saves: summary events (`storage_sheets.R`).
- Model runs: bulk synchronised when the existing **Load completed model runs** action reads the authoritative batch registry. **A publication-time pipeline hook is not yet installed**, so an untouched new run may not appear until that explicit sync or a historical import.
- Historical screening and adjudication progress: included in an explicit administrator-triggered import; **ongoing decision-by-decision progress events are not yet wired**.

**Backfill:** The **Historical import (administrator only)** action uses existing subset/model/comparison/conflict registries, restricted Zenodo prompt archive, assignments and decision logs. It bulk-appends metadata, skipping duplicate unchanged entity states. It is separate from fast overview refresh and not invoked automatically on login.

**Login:** Administrators receive an immediate `W04 · Screening and validation` landing page after authentication without eagerly loading all workflow queues. **Load full review task dashboard** performs the previous queue initialisation before reviewer work. The full task dashboard has a **W04 overview** return button. Non-administrator login paths remain unchanged. The old assignment administration interface still performs historical reads when opened; its later refactor remains pending.

**Validation:** [GitHub Actions run 39117622270](https://github.com/thesalmonandthetomato/LivingEvidenceMapAdjudication/actions/runs/39117622270) **PASS** for changed R file parsing, summary joins and admin deferred-loading guards; temporary workflow removed in commit `98414edb5155f283c9d9d56ffdf3cd552e0f451b`. This is **not live Google Sheets validation**.

**Immediate live checkpoint:**
1. Redeploy `main/multiuser_app/`, log in as administrator and confirm the W04 summary landing page appears promptly.
2. Open **Historical import**, run it once and wait for confirmation. It is expected to take longer because it retrieves the existing authoritative history.
3. Click **Refresh W04 overview**; confirm the four tables show subsets, prompts, model runs, comparisons and dates, without repeated Zenodo or decision-log reads.
4. Click **Load full review task dashboard**; confirm W01/W02/W04/W08 queues still work and assignments/decisions are intact.
5. Report any exact errors or slow step before proceeding to menu consolidation.

**Outstanding:** Live import and one-read speed verification; publication-time run-event integration; event-bound updates for human screening and adjudication progress; simplification of full assignment/Configure menus. Do not mark these complete before validation.
