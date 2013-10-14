#' Class representing a CSIDE module.
#' 
#' \code{Module$new()} initiates the module.
#' 
#' Info: All modules can be run in standalone mode (links back to the hub won't work of course, ideally should not appear in the GUI then)
#' \code{Module$new(standalone=TRUE)$showGUI()}
#' 
#' @param standalone a logical defining if the module should run in standalone mode (default: FALSE)
#' @param dirs all important folders to operate the module (home, projects, libs, settings, reports)
#' @method showGUI shows the user interface (requires a Hub object, unless running in standalone mode)
Module <- setRefClass(
  'Module', 
  fields = list(
    standalone = 'logical', 
    active = 'logical',
    dirs = 'list',
    settings = 'list',
    widgets = 'list',
    guiFunc = 'function' # function that takes parameters module and hub
    ), 
  methods = list(
     initialize = function(standalone = FALSE){
       callSuper()
       standalone <<- standalone
       active <<- FALSE
       dirs <<- list (
         home = getwd(), # home is the working directory by default
         projects = "projects", # data directories
         libs = "libraries", # compound libraries
         settings = "settings", # settings
         reports = "reports" # reports directories
       )
       settings <<- list(
         windowSize = c(800, 600), # default window size
         windowTitle = "Module" # default window title
       )
       guiFunc <<- function(module, hub) {} # empty GUI function 
     },
     
     #' Adjust settings - FIXME
     #addParams = function(...){
     #   params <<- modifyList(params, list(...))
     #},
     
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
       if (missing(hub)) hub <- NULL
       if (!standalone && (is.null(hub) || !is(hub, "Hub"))) stop("Module needs a hub to run in dependent mode. Provide a Hub or change module$standalone to TRUE.")
       active <<- TRUE # module becomes active
       
       # prepare GUI
       options("guiToolkit"="RGtk2") # everything is written in RGtk2
       cat("Showing GUI: ")
       print()
       
       # make new window if none exists yet for this module
       if (is.null(widgets$win)) {
          widgets$win <<- gwindow(settings$windowTitle, visible=FALSE, width=settings$windowSize[1], height=settings$windowSize[2])
          addHandlerUnrealize(widgets$win, handler = function(h,...) widgets$win <<- NULL) # record removal of the window
          do.call(guiFunc, args=list(module=.self, hub=hub))
       }
       
       visible(widgets$win, TRUE)
     },
     
     #' Method to hide the GUI (makes the module inactive)
     hideGUI = function(hub) {
       active <<- FALSE # module becomes inactive
       cat("Hiding GUI: ")
       print()
       if (!is.null(widgets$win)) visible(widgets$win, FALSE) # make window invisible
     },
     
     #' Check if module is active and stop if not
     checkActive = function() {
       if (!active) {
         message("Module is not currently active.\nCall stack:")
         catw() # writes out call stack - in utils.R
         stop("Call aborted.")
       }
     },
     
     ##################
     # Info messaging #
     ##################
     
     #' show an info message
     #' type - styling of the mssage, info, error, question, warning are the standard ones
     #' timer - time in seconds until message disappears automatically
     #' okButton - whether there is an ok button or not
     showInfo = function(msg, type="question", timer=2, okButton=TRUE) {
       checkActive()
       widgets$infoBar$setMessageType(type)
       widgets$infoLabel$setText(msg)
       widgets$infoBar$show()
       if (!okButton)
         widgets$infoOkButton$hide()  
       else
         widgets$infoOkButton$show()
       if (!is.null(timer)) {
         Sys.sleep(timer)
         IDP.hideInfo(idp)
       }
     },
     
     # hide info bar
     hideInfo = function() {
       checkActive()
       widgets$infoBar$hide()
     }
  )
)

#S4 definition to allow print on Module objects
print.Module <- function(module, ...) module$print(...) 