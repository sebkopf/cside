#' Class representing a CSIDE module.
#' \code{Module$new()} initiates the module.
#' @param standalone a logical defining if the module should run in standalone mode (default: FALSE)
#' @param dirs all important folders to operate the module (home, projects, libs, settings, reports)
#' @method showGUI shows the user interface (requires a Hub object, unless running in standalone mode)
Module <- setRefClass(
  'Module', 
  fields = list(
    standalone = 'logical', 
    active = 'logical',
    dirs = 'list'
    ), 
  methods = list(
     initialize = function(...){
       standalone <<- FALSE 
       active <<- FALSE
       dirs <<- list (
         home = getwd(), # home is the working directory by default
         projects = "projects", # data directories
         libs = "libraries", # compound libraries
         settings = "settings", # settings
         reports = "reports" # reports directories
       )
       callSuper(...)
     },
     
     #' Function to set a directory for the module.
     #' @param key which directory to set ("projects", "libs", "settings", "reports")
     #' @param dir the directory to set
     setDir = function(key, dir) dirs[[key]] <<- dir,
    
     #' Method to set home directory
     setHome = function(dir) setDir("home", dir),
     
     #' Method to print the status of the module
     print = function(...) {
       #FIXME
       cat("I am a", class(.self), "and I am active: ", active, "\n")
     },
     
     #' Method to show the GUI associated with the module (makes the module active)
     showGUI = function(hub) {
       if (!standalone && (missing(hub) || !is(hub, "Hub"))) stop("Module needs a hub to run in dependent mode. Provide a Hub or change module$standalone to TRUE.")
       active <<- TRUE # module becomes active
       cat("Showing GUI: ")
       print()
     },
     
     #' Method to hide the GUI (makes the module inactive)
     hideGUI = function(hub) {
       active <<- FALSE # module becomes inactive
       cat("Hiding GUI: ")
       print()
     }
  )
)

#S4 definition to allow print on Module objects
print.Module <- function(module, ...) module$print(...) 