source("R/search_source_selection.R")
catalogue <- '{"schema":"living-evidence-map-workflow00-source-groups-v1","groups":{"core":{"display_name":"Core","sources":["lens","scopus"]},"health":{"display_name":"Health","sources":["scopus","pubmed"]}}}'
groups <- search_source_groups_parse(catalogue)
stopifnot(identical(search_source_groups_resolve(groups,c("core","health")),
                    c("lens","scopus","pubmed")))
stopifnot(identical(search_source_groups_resolve(groups,character()),NULL) ||
          length(search_source_groups_resolve(groups,character()))==0L)
stopifnot(inherits(try(search_source_groups_resolve(groups,"unknown"),silent=TRUE),"try-error"))
stopifnot(inherits(try(search_source_groups_parse('{"groups":{}}'),silent=TRUE),"try-error"))
cat("PASS: group IDs resolved and overlapping sources deduplicated without W00 dispatch\n")
