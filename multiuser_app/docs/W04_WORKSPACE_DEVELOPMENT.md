# W04 Screening and Validation workspace

Status: Stage 1 COMPLETE (source-code inventory); Stage 2 NOT STARTED.
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
- [ ] **Stage 2: Read-only overview**. Bulk joined/cached views for subsets, prompt versions, model runs, saved consistency analyses; dates visible; validate in live Shiny; user checkpoint.
- [ ] **Stage 3: Consolidate W04 navigation**. Retain existing controls until new workspace is validated. Move W04-only controls out of configure/admin.
- [ ] **Stage 4: Simplify comparisons and conflict assignment**. Idempotent reuse of existing sets; preserve assignments/decision history; user checkpoint.
- [ ] **Stage 5: Event-bound refresh/caching**. Avoid polling and unnecessary Zenodo traffic. Assess materialised Sheets summary only if needed.
- [ ] **Stage 6: End-to-end validation and cleanup**. Regression, permissions, live verification, remove obsolete W04 UI.

## Progress reporting and handover
For each stage: record commit SHA, precise changes, validation links and pass/fail, production checks still needed, and next action. Stop at planned live user check-ins after Stage 2 and Stage 4. Never claim an automated test verifies live Google Sheets contents without a live test.

## Next action
Stage 2: add **isolated, read-only** W04 overview with authoritative timestamp columns (Subset created, Prompt saved, Model published, Comparison created); join from existing registry snapshots, use cached data with explicit refresh, and do not displace any currently working controls.
