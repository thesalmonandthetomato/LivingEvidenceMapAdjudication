source("R/search_boolean_validation.R")
valid <- c('salmon AND aquaculture', '("atlantic salmon" OR trout) AND farm*',
           'fish AND (feed OR NOT disease)', 'salmon')
invalid <- c('', 'AND salmon', 'salmon AND', 'salmon OR OR trout',
             'salmon trout', '(salmon OR trout', 'salmon )',
             'salmon AND ()', '"salmon')
for (x in valid) stopifnot(isTRUE(search_boolean_validate(x)$valid))
for (x in invalid) stopifnot(!isTRUE(search_boolean_validate(x)$valid))
cat("PASS: offline Searching Boolean cases; no provider-specific claims\n")
