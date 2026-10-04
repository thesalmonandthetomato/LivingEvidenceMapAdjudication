`%||%` <- function(x, y) if (is.null(x)) y else x

source("R/users.R")
source("R/decision_events.R")
source("R/assignments.R")
source("R/w04_blind_resolution.R")

assignments <- list(
  list(assignment_id="a1",workflow="04",task_type="manual_screening",batch_id="b4",case_id="c1",user_id="u1",blind_group="g",status="assigned"),
  list(assignment_id="a2",workflow="04",task_type="manual_screening",batch_id="b4",case_id="c1",user_id="u2",blind_group="g",status="assigned"),
  list(assignment_id="a3",workflow="04",task_type="manual_screening",batch_id="b4",case_id="c2",user_id="u1",blind_group="g",status="assigned"),
  list(assignment_id="a4",workflow="04",task_type="manual_screening",batch_id="b4",case_id="c2",user_id="u2",blind_group="g",status="assigned"),
  list(assignment_id="a5",workflow="04",task_type="manual_screening",batch_id="b4",case_id="c3",user_id="u1",blind_group="g",status="assigned"),
  list(assignment_id="a6",workflow="04",task_type="manual_screening",batch_id="b4",case_id="c3",user_id="u2",blind_group="g",status="assigned"),
  list(assignment_id="a7",workflow="04",task_type="manual_screening",batch_id="b4",case_id="c4",user_id="u1",blind_group="g",status="assigned"),
  list(assignment_id="a8",workflow="04",task_type="manual_screening",batch_id="b4",case_id="c4",user_id="u2",blind_group="g",status="assigned")
)

cases <- lapply(seq_len(5), function(i) list(review_case_id=paste0("c",i),record_id=paste0("r",i)))

decisions <- list(
  list(review_case_id="c1",reviewer="u1",decision="retain"),
  list(review_case_id="c1",reviewer="u2",decision="retain"),
  list(review_case_id="c2",reviewer="u1",decision="retain"),
  list(review_case_id="c2",reviewer="u2",decision="exclude"),
  list(review_case_id="c3",reviewer="u1",decision="uncertain"),
  list(review_case_id="c3",reviewer="u2",decision="uncertain"),
  list(review_case_id="c4",reviewer="u1",decision="exclude")
)

out <- w04_blind_case_outcomes(cases,assignments,decisions,"b4")
status <- setNames(vapply(out,function(x)x$status,character(1)),vapply(out,function(x)x$case_id,character(1)))
final <- setNames(vapply(out,function(x)x$final_decision,character(1)),vapply(out,function(x)x$case_id,character(1)))

stopifnot(
  identical(status[["c1"]],"agreement"),
  identical(final[["c1"]],"retain"),
  identical(status[["c2"]],"conflict"),
  identical(status[["c3"]],"conflict"),
  identical(status[["c4"]],"pending"),
  identical(status[["c5"]],"unassigned"),
  length(w04_blind_agreements(out)) == 1L,
  length(w04_blind_conflicts(out)) == 2L,
  length(w04_blind_pending(out)) == 1L
)

cat("PASS: W04 blind review outcomes distinguish agreement, conflict, pending and unassigned\n")
