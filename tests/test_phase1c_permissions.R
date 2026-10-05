source("R/adjudication_schema.R")
source("R/users.R")

admin <- list(
  user_id = "usr-admin",
  email = "admin@example.org",
  display_name = "Administrator",
  role = "administrator",
  active = TRUE
)

reviewer <- list(
  user_id = "usr-reviewer",
  email = "reviewer@example.org",
  display_name = "Reviewer",
  role = "reviewer",
  active = TRUE
)

stopifnot(user_can(admin, "adjudicate_assigned"))
stopifnot(user_can(admin, "resolve_conflicts"))
stopifnot(user_can(admin, "manage_assignments"))
stopifnot(user_can(admin, "control_workflows"))
stopifnot(user_can(admin, "export_to_github"))
stopifnot(user_can(admin, "manage_users"))

stopifnot(user_can(reviewer, "adjudicate_assigned"))
stopifnot(user_can(reviewer, "resolve_conflicts"))
stopifnot(!user_can(reviewer, "manage_assignments"))
stopifnot(!user_can(reviewer, "control_workflows"))
stopifnot(!user_can(reviewer, "export_to_github"))
stopifnot(!user_can(reviewer, "manage_users"))

app <- paste(readLines("app.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

count_pattern <- function(pattern) {
  m <- gregexpr(pattern, app, perl = TRUE)[[1L]]
  if (length(m) == 1L && identical(m[[1L]], -1L)) 0L else length(m)
}

# Every current adjudication surface must enforce adjudication permission.
stopifnot(count_pattern('session_can\\("adjudicate_assigned"\\)') >= 4L)

# Workflow-state changes and downstream dispatches must remain administrator-only.
stopifnot(count_pattern('session_can\\("control_workflows"\\)') >= 5L)

stopifnot(grepl(
  'dispatch_completed_w02 <- function\\(\\) \\{\\s*if\\(!session_can\\("control_workflows"\\)\\)',
  app, perl = TRUE
))

w04_finalize_start <- regexpr(
  "dispatch_completed_w04_validation <- function()",
  app,
  fixed = TRUE
)[[1L]]
w04_finalize_call <- regexpr(
  "dispatch_w04_validation_finalize(",
  app,
  fixed = TRUE
)[[1L]]
stopifnot(w04_finalize_start > 0L, w04_finalize_call > w04_finalize_start)
w04_finalize_block <- substr(app, w04_finalize_start, w04_finalize_call)
stopifnot(grepl('session_can\\("control_workflows"\\)', w04_finalize_block, perl = TRUE))

for (dispatch_call in c(
  "dispatch_w04_resolution_resume",
  "dispatch_w08_resume"
)) {
  pos <- regexpr(dispatch_call, app, fixed = TRUE)[[1L]]
  stopifnot(pos > 0L)
  before <- substr(app, max(1L, pos - 500L), pos)
  stopifnot(grepl('session_can\\("control_workflows"\\)', before, perl = TRUE))
}

cat("PASS: Phase 1C role enforcement\n")
