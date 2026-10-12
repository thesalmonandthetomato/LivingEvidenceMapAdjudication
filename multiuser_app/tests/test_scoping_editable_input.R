# Static regression guard for the deployed Shiny scoping interface.
app <- paste(readLines("app.R",warn=FALSE),collapse="\n")
stopifnot(!grepl('read_github_text_file("user_input/scoping_search_string.txt")',app,fixed=TRUE))
stopifnot(grepl('"search_scope_input_text","Scoping search string"',app,fixed=TRUE))
stopifnot(grepl('isTRUE(search_boolean_validate(search_scope_string_rv())$valid)',app,fixed=TRUE))
stopifnot(grepl('if (!isTRUE(search_boolean_validate(query)$valid))',app,fixed=TRUE))
stopifnot(grepl('identical(as.character(current_user()$role),"administrator")',app,fixed=TRUE))
cat("PASS: scoping starts blank, is admin-editable, and validates before dispatch\n")
