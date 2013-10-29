#' Class representing a CSIDE module.
#' Basically a data frame that has a user interface associated with it.
#' 
#' \code{Module$new()} initiates the module.
#' 
#' @param dirs all important folders to operate the module (home, projects, libs, settings, reports)
#' @method showGUI shows the user interface (requires a Hub object, unless running in standalone mode)
Module <- setRefClass(
  'Module',
  contains = 'DataFrame',
  fields = list(
    gui = 'GUI', # an S4 gui class
    widgets = 'list' # list of all the widgets to keep track of
  ), 
  methods = list(
     initialize = function(...){
       callSuper(...)
       
       ### default setting for a module
       setSettings(
         windowSize = c(800, 600),
         windowTitle = "Module",
         launchIcon = "gtk-yes",
         launchName = "Module",
         launchTooltip = "Start this module"
       )
     },
    
     getWidget = function(id) {
       return (widgets[[id]])
     },
     
     getWidgets = function(ids) {
       if (missing(ids))
         return (widgets)
       return (widgets[ids])
     },
     
     setWidgets = function(...) {
       # FIXME: allow both ... and list(a=b) to be passed in!
       widgets <<- modifyList(widgets, list(...))
     },
     
     cleanWidgets = function() {
       widgets <<- list()
     }
      
     
#      #' Function to set a directory for the module.
#      #' @param key which directory to set ("projects", "libs", "settings", "reports")
#      #' @param dir the directory to set
#      setDir = function(key, dir) dirs[[key]] <<- dir,
#     
#      #' Method to set home directory
#      setHome = function(dir) setDir("home", dir),
#      
#      #' Method to print the status of the module
#      print = function(...) {
#        #FIXME
#        cat("I am a", class(.self), "and I am active: ", active, "and have the GUI", class(GUI), "\n")
#      },
     
  )
)

#S4 definition to allow print on Module objects
# --> can then be called by print(module) 
print.Module <- function(module, ...) module$print(...) 