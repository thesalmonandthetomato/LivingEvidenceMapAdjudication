storage_backend <- function() {
  tolower(Sys.getenv("LEM_STORAGE_BACKEND", unset = "local"))
}

read_active_decisions <- function(local_path = NULL) {
  b <- storage_backend()
  if (identical(b, "google_sheets")) return(active_sheet_decisions())
  if (!identical(b, "local")) stop("Unsupported LEM_STORAGE_BACKEND: ", b, call.=FALSE)
  read_local_decisions(local_path)
}

save_active_decision <- function(decision, local_path = NULL, prior_decision = NULL) {
  b <- storage_backend()
  if (identical(b, "google_sheets")) return(append_sheet_decision(decision, prior_decision = prior_decision))
  if (!identical(b, "local")) stop("Unsupported LEM_STORAGE_BACKEND: ", b, call.=FALSE)
  write_local_decision(local_path, decision)
}


read_user_registry <- function(local_path = NULL) {
  b <- storage_backend()
  users <- if (identical(b, "google_sheets")) {
    read_sheet_users()
  } else {
    if (!identical(b, "local")) stop("Unsupported LEM_STORAGE_BACKEND: ", b, call. = FALSE)
    read_local_users(local_path)
  }

  if (!length(users)) users <- initial_users_from_env()
  validate_user_registry(users)
  users
}


read_assignment_registry <- function(local_path = NULL) {
  b <- storage_backend()
  if (identical(b, "local")) return(read_local_assignments(local_path))
  if (identical(b, "google_sheets")) return(read_sheet_assignments(create_if_missing = TRUE))
  stop("Unsupported LEM_STORAGE_BACKEND: ", b, call. = FALSE)
}


save_assignment_registry <- function(assignments, local_path = NULL, actor_user_id = "") {
  b <- storage_backend()
  if (identical(b, "local")) return(write_local_assignments(local_path, assignments))
  if (identical(b, "google_sheets")) {
    return(write_sheet_assignments(assignments, actor_user_id = actor_user_id))
  }
  stop("Unsupported LEM_STORAGE_BACKEND: ", b, call. = FALSE)
}
