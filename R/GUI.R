# Generic functions that GUI classes can attach to
setGeneric("makeGUI", function(object, hub) standardGeneric("makeGUI"))
setGeneric("destroyGUI", function(object, hub) standardGeneric("destroyGUI"))
setGeneric("remakeGUI", function(object, hub) standardGeneric("remakeGUI"))

setGeneric("showGUI", function(object, hub) standardGeneric("showGUI"))
setGeneric("hideGUI", function(object, hub) standardGeneric("hideGUI"))

# convenience for interacting with the hub
setGeneric("getWidget", function(object, hub, id) standardGeneric("getWidget"))
setGeneric("getWindow", function(object, hub) standardGeneric("getWindow"))

###########
# Methods #
###########


# S4 class GUI
GUI <- setClass("GUI", 
                representation(module="character")) # which module this belongs to, is automatically set by the hub

setMethod("makeGUI", "GUI", function(object, hub) {
  # prepare GUI
  options("guiToolkit"="RGtk2") # everything is written in RGtk2
  
  print("making GUI")
})

setMethod("destroyGUI", "GUI", function(object, hub) {
  print("destroying GUI")
})

setMethod("remakeGUI", "GUI", function(object, hub) {
  print("remaking GUI")
})

setMethod("showGUI", "GUI", function(object, hub) {
  if (is.null(getWindow(object)))
    
  cat("I am a", class(object), "and I am showing GUI.\n")
})

setMethod("hideGUI", "GUI", function(object, hub) {
  cat("I am a", class(object), "and I am hiding GUI.\n")
})



######################################################
# Convenience functions for interacting with the hub #
######################################################

setMethod("getWidget", "GUI", function(object, hub, id) {
  return(hub$getModule()$getWidget(id))
})

setMethod("getWindow", "GUI", function(object, hub) {
  return(hub$getModule()$getWidget("window"))
})


# 
# # GUI utility functions
# guiQuit <- function(module, hub) {
#   ## FIXME: implement proper shutdown triggered over modules and hub to ask for saving settings
#   # get out of modal loop, etc.
#   dispose(module$widgets$win)
# }
# if (is.null(widgets$win)) {
#   widgets$win <<- gwindow(settings$windowTitle, visible=FALSE, width=settings$windowSize[1], height=settings$windowSize[2])
#   addHandlerUnrealize(widgets$win, handler = function(h,...) widgets$win <<- NULL) # record removal of the window
#   do.call(guiFunc, args=list(module=.self, hub=hub))
# }
# 
# #' Method to hide the GUI (makes the module inactive)
# hideGUI = function(hub) {
#   if (!is(hub, "Hub"))) stop("Module needs a hub to run.")
# active <<- FALSE # module becomes inactive
# if (!is.null(widgets$win)) visible(widgets$win, FALSE) # make window invisible
# },

##################
# Info messaging #
##################

#' show an info message
#' type - styling of the mssage, info, error, question, warning are the standard ones
#' timer - time in seconds until message disappears automatically
# #' okButton - whether there is an ok button or not
# showInfo = function(msg, type="question", timer=2, okButton=TRUE) {
#   checkActive()
#   widgets$infoBar$setMessageType(type)
#   widgets$infoLabel$setText(msg)
#   widgets$infoBar$show()
#   if (!okButton)
#     widgets$infoOkButton$hide()  
#   else
#     widgets$infoOkButton$show()
#   if (!is.null(timer)) {
#     Sys.sleep(timer)
#     IDP.hideInfo(idp)
#   }
# }
# 
# # hide info bar
# hideInfo = function() {
#   checkActive()
#   widgets$infoBar$hide()
# }
