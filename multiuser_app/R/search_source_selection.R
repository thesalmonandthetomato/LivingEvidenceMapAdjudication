# Offline source-group resolver: uses canonical W00 group schema and IDs.
search_source_groups_parse <- function(json_text) {
  cfg <- jsonlite::fromJSON(json_text,simplifyVector=FALSE)
  if(!identical(cfg$schema,"living-evidence-map-workflow00-source-groups-v1") ||
     !is.list(cfg$groups) || !length(cfg$groups))
    stop("Unsupported W00 source group catalogue",call.=FALSE)
  for(id in names(cfg$groups)) {
    entry <- cfg$groups[[id]]
    if(!nzchar(as.character(entry$display_name %||% "")) ||
       !is.character(unlist(entry$sources,use.names=FALSE)) ||
       !length(entry$sources))
      stop(paste("Invalid source group",id),call.=FALSE)
  }
  cfg$groups
}
search_source_groups_choices <- function(groups) {
  stats::setNames(names(groups),vapply(groups,function(g)as.character(g$display_name),character(1)))
}
search_source_groups_resolve <- function(groups, selected) {
  if(any(!selected %in% names(groups)))
    stop("Unknown source group",call.=FALSE)
  unique(unlist(lapply(selected,function(id)unlist(groups[[id]]$sources,use.names=FALSE)),use.names=FALSE))
}
