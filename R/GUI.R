# Generic functions that GUI classes can attach to

# making the gui
setGeneric("makeGUI", function(gui, hub) standardGeneric("makeGUI"))
setGeneric("destroyGUI", function(gui, hub) standardGeneric("destroyGUI"))
setGeneric("remakeGUI", function(gui, hub, show) standardGeneric("remakeGUI"))
setGeneric("showGUI", function(gui, hub) standardGeneric("showGUI"))
setGeneric("hideGUI", function(gui, hub) standardGeneric("hideGUI"))

# functions that are intended to be extended in GUI derived classes
setGeneric("makeMenu", function(gui, hub) standardGeneric("makeMenu"))
setGeneric("makeToolbar", function(gui, hub) standardGeneric("makeToolbar"))
setGeneric("makeMainGUI", function(gui, hub) standardGeneric("makeMainGUI"))

# convenience for interacting with the hub
setGeneric("getModule", function(gui, hub, id) standardGeneric("getModule"))
setGeneric("getWidget", function(gui, hub, id) standardGeneric("getWidget"))
setGeneric("getWindow", function(gui, hub) standardGeneric("getWindow"))
setGeneric("setWidgets", function(gui, hub, ...) standardGeneric("setWidgets"))
setGeneric("getSetting", function(gui, hub, id) standardGeneric("getSetting"))

#########
# Class #
#########

# S4 class GUI
GUI <- setClass("GUI", representation = list(module="character")) # which module this belongs to, is automatically set by the hub

setMethod("initialize", "GUI", function(.Object, ...) {
  callNextMethod(.Object, ...)
})

###################
# general Methods #
###################

setMethod("makeGUI", "GUI", function(gui, hub) {
  cat("\tI am a", class(gui), "and have module", gui@module, "and I am making GUI.\n")
  options("guiToolkit"="RGtk2") # everything is written in RGtk2
  
  cat("\tGUI settings are as follows:\n")
  print(getModule(gui, hub)$getSettings())
  
  # make window
  win <- gwindow(
    getSetting(gui, hub, "windowTitle"), visible=FALSE, 
    width=getSetting(gui, hub, "windowSize")[1], 
    height=getSetting(gui, hub, "windowSize")[2])
  
  # destroy window properly
  addHandlerDestroy(win, function(h,...) {
      cat(paste0("Window for ", class(gui), " (module ", gui@module,") is being destroyed! Cleaning all widgets in the module.\n"))
      getModule(gui, hub)$cleanWidgets() # clean all widget references
    }) # clean widgets
  
  # save window in hub
  setWidgets(gui, hub, window = win)
  
  # make GUI
  makeMenu(gui, hub)
  makeToolbar(gui, hub)
  makeMainGUI(gui, hub)
})

setMethod("destroyGUI", "GUI", function(gui, hub) {
  cat("I am a", class(gui), "and have module", gui@module, "and I am destroying GUI.\n")
  dispose(getWindow(gui, hub)) # destroy window
  getModule(gui, hub)$cleanWidgets() # clean all widget references
})

setMethod("remakeGUI", "GUI", function(gui, hub, show = TRUE) {
  destroyGUI(gui, hub)
  makeGUI(gui, hub)
  if (show)
    showGUI(gui, hub)
})

setMethod("showGUI", "GUI", function(gui, hub) {
  cat("I am a", class(gui), "and have module", gui@module, "and I am showing GUI.\n")
  if (is.null(getWindow(gui, hub)))
    makeGUI(gui, hub)
  visible(getWindow(gui, hub), TRUE)
})

setMethod("hideGUI", "GUI", function(gui, hub) {
  cat("I am a", class(gui), "and have module", gui@module, "and I am hiding GUI.\n")
  if (!is.null(getWindow(gui, hub)))
    visible(getWindow(gui, hub), FALSE)
})

############################################
# methods that are supposed to be extended #
############################################

setMethod("makeMenu", "GUI", function(gui, hub) {})
setMethod("makeToolbar", "GUI", function(gui, hub) {})
setMethod("makeMainGUI", "GUI", function(gui, hub) {})

######################################################
# Convenience functions for interacting with the hub #
######################################################

setMethod("getModule", "GUI", function(gui, hub) {
  return(hub$getModule(gui@module))
})

setMethod("getWidget", "GUI", function(gui, hub, id) {
  return(getModule(gui, hub)$getWidget(id))
})

setMethod("getWindow", "GUI", function(gui, hub) {
  return(getModule(gui, hub)$getWidget("window"))
})

setMethod("setWidgets", "GUI", function(gui, hub, ...) {
  return(getModule(gui, hub)$setWidgets(...))
})

setMethod("getSetting", "GUI", function(gui, hub, id) {
  return(getModule(gui, hub)$getSetting(id))
})

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
