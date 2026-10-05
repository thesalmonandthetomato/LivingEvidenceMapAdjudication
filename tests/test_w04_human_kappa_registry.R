source("R/w01_contract.R")
source("R/adjudication_schema.R")
source("R/assignments.R")
source("R/w04_blind_resolution.R")
source("R/w04_human_kappa_registry.R")

make_assignment <- function(cid,uid,i) list(
  assignment_id=paste0("asg-",i,"-",uid),
  workflow="04",task_type="manual_screening",batch_id="b1",
  case_id=cid,user_id=uid,blind_group="w04-reviewer-consistency",
  status="assigned"
)
assignments <- list()
k <- 0L
for (cid in paste0("c",1:4)) for (uid in c("matt","sini")) {
  k <- k+1L
  assignments[[k]] <- make_assignment(cid,uid,k)
}

make_outcome <- function(cid,a,b,complete=TRUE) list(
  case_id=cid,
  reviewer_decisions=list(
    matt=list(complete=complete,decision=if(complete)a else ""),
    sini=list(complete=complete,decision=if(complete)b else "")
  )
)
outcomes_100 <- list(
  make_outcome("c1","retain","retain"),
  make_outcome("c2","retain","exclude"),
  make_outcome("c3","exclude","exclude",FALSE),
  make_outcome("c4","exclude","retain",FALSE)
)
outcomes_200 <- list(
  make_outcome("c1","retain","retain"),
  make_outcome("c2","retain","exclude"),
  make_outcome("c3","exclude","exclude"),
  make_outcome("c4","exclude","retain")
)

empty <- w04_empty_human_kappa_registry()
s1 <- w04_human_consistency_scope(
  outcomes_100,assignments,"b1",c("sini","matt"),empty,"partial"
)
stopifnot(isTRUE(s1$save_ready),length(s1$target_case_ids)==2L,s1$analysis$complete==2L)
row1 <- w04_human_registry_row(
  s1$analysis,s1$target_case_ids,"b1",paste(rep("a",64),collapse=""),
  s1$assignment_scope_id,"partial",c("Matt","Sini"),created_at_utc="2026-10-05T10:00:00Z"
)
reg1 <- row1

s2 <- w04_human_consistency_scope(
  outcomes_200,assignments,"b1",c("matt","sini"),reg1,"partial"
)
stopifnot(isTRUE(s2$save_ready),setequal(s2$target_case_ids,c("c3","c4")))
row2 <- w04_human_registry_row(
  s2$analysis,s2$target_case_ids,"b1",paste(rep("a",64),collapse=""),
  s2$assignment_scope_id,"partial",c("Matt","Sini"),created_at_utc="2026-10-05T11:00:00Z"
)
reg2 <- rbind(reg1,row2)
cum2 <- w04_human_registry_cumulative(reg2,w04_human_reviewer_key(c("matt","sini")))
stopifnot(isTRUE(cum2$valid),cum2$n==4L,length(cum2$contributing_ids)==2L)

sf <- w04_human_consistency_scope(
  outcomes_200,assignments,"b1",c("matt","sini"),reg2,"full"
)
stopifnot(isTRUE(sf$save_ready),length(sf$target_case_ids)==4L)
rowf <- w04_human_registry_row(
  sf$analysis,sf$target_case_ids,"b1",paste(rep("a",64),collapse=""),
  sf$assignment_scope_id,"full",c("Matt","Sini"),created_at_utc="2026-10-05T12:00:00Z"
)
reg3 <- rbind(reg2,rowf)
cum3 <- w04_human_registry_cumulative(reg3,w04_human_reviewer_key(c("matt","sini")))
stopifnot(isTRUE(cum3$valid),cum3$n==4L,length(cum3$contributing_ids)==1L)
stopifnot(length(cum3$superseded_ids)==2L,identical(cum3$contributing_ids,rowf$consistency_id))

s_after <- w04_human_consistency_scope(
  outcomes_200,assignments,"b1",c("matt","sini"),reg3,"partial"
)
stopifnot(!isTRUE(s_after$save_ready),length(s_after$target_case_ids)==0L)

# Three-human Fleiss kappa can be combined exactly from stored pattern counts.
make3 <- function(cid,a,b,c) list(
  case_id=cid,
  reviewer_decisions=list(
    a=list(complete=TRUE,decision=a),
    b=list(complete=TRUE,decision=b),
    c=list(complete=TRUE,decision=c)
  )
)
# Avoid name/value collision by constructing explicitly.
three <- list(
  list(case_id="t1",reviewer_decisions=list(a=list(complete=TRUE,decision="retain"),b=list(complete=TRUE,decision="retain"),c=list(complete=TRUE,decision="retain"))),
  list(case_id="t2",reviewer_decisions=list(a=list(complete=TRUE,decision="retain"),b=list(complete=TRUE,decision="exclude"),c=list(complete=TRUE,decision="exclude"))),
  list(case_id="t3",reviewer_decisions=list(a=list(complete=TRUE,decision="exclude"),b=list(complete=TRUE,decision="exclude"),c=list(complete=TRUE,decision="exclude"))),
  list(case_id="t4",reviewer_decisions=list(a=list(complete=TRUE,decision="uncertain"),b=list(complete=TRUE,decision="uncertain"),c=list(complete=TRUE,decision="exclude")))
)
a1 <- w04_consistency_analysis(three[1:2],c("a","b","c"))
a2 <- w04_consistency_analysis(three[3:4],c("a","b","c"))
r1 <- w04_human_registry_row(a1,c("t1","t2"),"b2",paste(rep("b",64),collapse=""),"s2","partial",c("A","B","C"),created_at_utc="2026-10-06T10:00:00Z")
r2 <- w04_human_registry_row(a2,c("t3","t4"),"b2",paste(rep("b",64),collapse=""),"s2","partial",c("A","B","C"),created_at_utc="2026-10-06T11:00:00Z")
cf <- w04_human_registry_cumulative(rbind(r1,r2),w04_human_reviewer_key(c("a","b","c")))
direct <- w04_consistency_analysis(three,c("a","b","c"))
stopifnot(isTRUE(cf$valid),cf$n==4L,abs(cf$kappa-direct$kappa)<1e-12)

cat("PASS: W04 human consistency Partial/Full scope, supersession and cumulative kappa\n")
