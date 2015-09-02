#' @include CsideHub.R
NULL

cside <- function(
  home = getwd(),
  dataModules = list(
    gcms = GcMsFidModule$new(),
    dh = DHModule$new()
  ),
  debug = FALSE) {
  
  cat("\nInitializing CSIDE ...\n")
  DEBUG <<- debug # debug messages
  
  # make sure the right id function (from the widgets package) is used
  id <- gWidgets::id
  
  # initialize CSIDE hub
  hub <- CsideHub$new(dataModules = dataModules, home = home)
  hub$launchHub()
  return(invisible(hub))
}

cside.dev <- function() {
  cside(debug = TRUE)
}