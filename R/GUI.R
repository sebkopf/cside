# Generic functions that GUI classes can attach to

# convenience for interacting with the hub
setGeneric("getModule", function(gui, hub, id) standardGeneric("getModule"))
setGeneric("getWidget", function(gui, hub, id) standardGeneric("getWidget"))
setGeneric("setWidgets", function(gui, hub, ...) standardGeneric("setWidgets"))
setGeneric("getSetting", function(gui, hub, id) standardGeneric("getSetting"))

# making the gui
setGeneric("makeGUI", function(gui, hub) standardGeneric("makeGUI"))
setGeneric("destroyGUI", function(gui, hub) standardGeneric("destroyGUI"))
setGeneric("remakeGUI", function(gui, hub, show = TRUE) standardGeneric("remakeGUI"))
setGeneric("showGUI", function(gui, hub) standardGeneric("showGUI"))
setGeneric("hideGUI", function(gui, hub) standardGeneric("hideGUI"))

# specific functions streamlining GUI design but that are not intended to be derived
setGeneric("getNavigationXML", function(gui, hub) standardGeneric("getNavigationXML"))
setGeneric("makeNavigation", function(gui, hub) standardGeneric("makeNavigation"))

# getting and setting key widgets
setGeneric("getWindow", function(gui, hub) standardGeneric("getWindow"))
setGeneric("setMenuGroup", function(gui, hub, menuGroup) standardGeneric("setMenuGroup"))
setGeneric("setToolbarGroup", function(gui, hub, toolbarGroup) standardGeneric("setToolbarGroup"))

# functions that are intended to be extended in GUI derived classes
setGeneric("getMenuXML", function(gui, hub) standardGeneric("getMenuXML"))
setGeneric("getToolbarXML", function(gui, hub) standardGeneric("getToolbarXML"))
setGeneric("makeMainGUI", function(gui, hub) standardGeneric("makeMainGUI"))
setGeneric("setNavigationActions", function(gui, hub, actionGrp) standardGeneric("setNavigationActions"))

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
  cat("I am a", class(gui), "and have module", gui@module, "and I am making my GUI.\n")
  options("guiToolkit"="RGtk2") # everything is written in RGtk2
  
  cat("\tGUI settings are as follows:\n")
  
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
  makeMainGUI(gui, hub)
  makeNavigation(gui, hub)
})

setMethod("destroyGUI", "GUI", function(gui, hub) {
  cat("I am a", class(gui), "and have module", gui@module, "and I am destroying my GUI.\n")
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
  cat("I am a", class(gui), "and have module", gui@module, "and I am showing my GUI.\n")
  if (is.null(getWindow(gui, hub)))
    makeGUI(gui, hub)
  visible(getWindow(gui, hub), TRUE)
})

setMethod("hideGUI", "GUI", function(gui, hub) {
  cat("I am a", class(gui), "and have module", gui@module, "and I am hiding my GUI.\n")
  if (!is.null(getWindow(gui, hub)))
    visible(getWindow(gui, hub), FALSE)
})

################################
# methods for streamlining GUI #
################################

setMethod("getNavigationXML", "GUI", function(gui, hub) {
  return(paste(
    '<ui>',
      '<menubar name="menubar">',
        getMenuXML(gui, hub),      
      '</menubar>',
      '<toolbar name ="toolbar">',
        getToolbarXML(gui, hub),
      '</toolbar>',
    '</ui>', sep="\n"))
})

setMethod("makeNavigation", "GUI", function(gui, hub) {
  
  # navigation actions
  cat("\tInitializing Navigation.\n")
  setWidgets(gui, hub, actionGroup = gtkActionGroup ("FileGroup"))
  cat("\tSetting Navigation Actions.\n")
  setNavigationActions(gui, hub, getWidget(gui, hub, 'actionGroup'))
  
  # UI manager for navigation
  cat("\tMaking Navigation Manager.\n")
  uimanager <- gtkUIManagerNew() # ui manager
  uimanager$insertActionGroup (getWidget(gui, hub, 'actionGroup'), 0) # add actions
  uimanager$addUiFromString (getNavigationXML(gui, hub)) # add ui 
  
  # menu
  menuGrp <- getWidget(gui, hub, 'menuGroup')
  if (!is.null(menuGrp)) {
    cat("\tMaking Menubar.\n")
    getToolkitWidget(menuGrp)$packStart (uimanager$getWidget ("/menubar"), FALSE ) # add menu
  }
    
  # toolbar
  toolbarGrp <- getWidget(gui, hub, 'toolbarGroup')
  if (!is.null(toolbarGrp)) {
    getToolkitWidget(toolbarGrp)$packStart (uimanager$getWidget ( "/toolbar" ), FALSE) # add toolbar
    cat("\tMaking Toolbar.\n")
  }
  getToolkitWidget(getWindow(gui, hub))$addAccelGroup (uimanager$getAccelGroup()) # add keyboard triggers
  
})

############################################
# methods that are supposed to be extended #
############################################

setMethod("getMenuXML", "GUI", function(gui, hub) { return('') })

setMethod("getToolbarXML", "GUI", function(gui, hub) { return('') })

setMethod("makeMainGUI", "GUI", function(gui, hub) {})

setMethod("setNavigationActions", "GUI", function(gui, hub, actionGrp) { })

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

###################################
# Getting and Setting key widgets #
###################################

setMethod("setMenuGroup", "GUI", function(gui, hub, menuGroup) {
  setWidgets(gui, hub, menuGroup = menuGroup)
})

setMethod("setToolbarGroup", "GUI", function(gui, hub, toolbarGroup) {
  setWidgets(gui, hub, toolbarGroup = toolbarGroup)
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

