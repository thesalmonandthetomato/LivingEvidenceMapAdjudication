# Offline, topic-independent Boolean expression checker for the Searching draft.
# This checks structural syntax; database-specific query compatibility is separate.
search_boolean_validate <- function(value) {
  fail <- function(message) list(valid=FALSE, message=message)
  if (!is.character(value) || length(value)!=1L || is.na(value) ||
      !nzchar(trimws(value))) return(fail("Enter a search string."))
  chars <- strsplit(value, "", fixed=TRUE)[[1L]]
  n <- length(chars)
  tokens <- character()
  positions <- integer()
  i <- 1L
  while (i <= n) {
    ch <- chars[[i]]
    if (grepl("^[[:space:]]$", ch)) {i <- i+1L; next}
    pos <- i
    if (ch %in% c("(", ")")) {
      tokens <- c(tokens, ch)
      positions <- c(positions, pos)
      i <- i+1L
      next
    }
    quoted <- FALSE
    escaped <- FALSE
    word <- ""
    while (i <= n) {
      ch <- chars[[i]]
      if (!quoted && (grepl("^[[:space:]]$",ch) || ch %in% c("(",")"))) break
      if (escaped) {
        escaped <- FALSE
      } else if (quoted && ch == "\\") {
        escaped <- TRUE
      } else if (ch == '"') {
        quoted <- !quoted
      }
      word <- paste0(word, ch)
      i <- i+1L
    }
    if (quoted) return(fail(sprintf("Unclosed double quote starting at character %d.", pos)))
    tokens <- c(tokens, word)
    positions <- c(positions, pos)
  }
  depth <- 0L
  expecting_operand <- TRUE
  for (k in seq_along(tokens)) {
    token <- tokens[[k]]
    op <- toupper(token)
    pos <- positions[[k]]
    if (token == "(") {
      if (!expecting_operand) return(fail(sprintf("Missing operator before '(' at character %d.",pos)))
      depth <- depth+1L
    } else if (token == ")") {
      if (expecting_operand) return(fail(sprintf("Missing operand before ')' at character %d.",pos)))
      depth <- depth-1L
      if (depth < 0L) return(fail(sprintf("Unexpected ')' at character %d.",pos)))
      expecting_operand <- FALSE
    } else if (op %in% c("AND","OR","NOT")) {
      if (op == "NOT") {
        if (!expecting_operand) return(fail(sprintf("Missing operator before NOT at character %d.",pos)))
      } else {
        if (expecting_operand) return(fail(sprintf("Missing left operand for %s at character %d.",token,pos)))
        expecting_operand <- TRUE
      }
    } else {
      if (!expecting_operand) return(fail(sprintf("Missing Boolean operator at character %d.",pos)))
      expecting_operand <- FALSE
    }
  }
  if (depth != 0L) return(fail(sprintf("%d unmatched opening parenthesis(es).",depth)))
  if (expecting_operand) return(fail("Boolean expression ends with an operator or missing operand."))
  list(valid=TRUE,message="Boolean structure valid. Database-specific syntax is not checked.")
}
