#' @include GuiModule.R

#' The start screen of cside
CsideStartModule = setRefClass(
  'CsideStartModule', contains='GuiModule', 
  methods = list(
    initialize = function(...){
      # define GUI class
      callSuper(gui = CsideStartGui(), ...)
      
      # settings
      setSettings(
        windowSize = c(400, 400),
        windowTitle = "CSIDE"
      )
    }
  )
)


#' S4 class CsideStartGui
#' The parameter 'module' in these classes is always actually a hub that overrides the getModule method
#' to return the correct module for the Gui.
CsideStartGui <- setClass("CsideStartGui", contains="BaseGui")

setMethod("getMenuXML", "CsideStartGui", function(gui, module) {
  return (
    '<menu name = "CSIDE" action="CSIDE">
    <menuitem action="Help"/>
    <menuitem action="Reload"/>
    <menuitem action="Quit" />
    </menu>')
})

setMethod("getToolbarXML", "CsideStartGui", function(gui, module) {
  nav <- '<separator expand="true"/>'
  for (name in names(module$modules)) # add all modules to toolbar
    if (name!='start')
      nav <- paste0(nav, '<toolitem action="Module', name,'"/>')
  return (paste0(nav, '<separator expand="true"/>'))
})

setMethod("setNavigationActions", "CsideStartGui", function(gui, module, actionGrp) {
  # 'module' is really the hub here
  hub <- module
  
  nav.actions <-
    list(## name, icon, label , accelerator , tooltip , callback
      list ("CSIDE" , NULL , "_CSIDE" , NULL , NULL , NULL ) ,
      list ("Help" , "gtk-info" ,"Help" , "<ctrl>H" , NULL , function(...) gmessage("sorry, not implemented yet") ) ,
      list ("Reload" , "gtk-refresh" ,"Reload Screen" , "<ctrl>R" , NULL , function(...) remakeGui(gui, module) ) , # FIXME: disable the keyboard shortcut!
      list ("Quit", "gtk-quit", "Quit", "<ctrl>Q", "Quit program", function(...) destroyGui(gui, module) ))
  
  # without this construct, only evaluates the name upon activation
  launch_module <- function(x) {
    force(x) 
    return(function(...) module$launchModule(x))
  }
  
  # add module launch actions to toolbar
  for (name in names(hub$modules)) {
    if (name!='start') {
      nav.actions <- c(nav.actions, list(
        list(paste0("Module", name), # action handle
             hub$getModule(name)$getSettings("launchIcon"), # icon
             hub$getModule(name)$getSettings("launchName"), # label
             NULL, # short cut
             hub$getModule(name)$getSettings("launchTooltip"), # tooltip
             launch_module(name)))) # launch module
    }
  }
  actionGrp$addActions(nav.actions)
})

setMethod("makeMainGui", "CsideStartGui", function(gui, module) {
  # 'module' is really the hub here
  hub <- module
  
  # top level groups
  setMenuGroup(gui, module, ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0))
  mainGrp <- ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0, expand=TRUE)
  infoGrp <- ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0)
  setToolbarGroup(gui, module, ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0))
  
  # working directory selection
  # file browser
  fileBrowser.items <- function(path = NULL, user.data=NULL) {
    if (is.null(path)) 
      path <- hub$getHome()
    else
      path <- file.path(hub$getHome(), do.call("file.path", args=as.list(path)))
    
    hub$setWD(path)
    showInfo(gui, module, hub$getWD(), timer=NULL, okButton=FALSE)
    
    folders <- subset(data.frame(
      Folder=dir(path, include.dirs=TRUE),
      Path=dir(path, include.dirs=TRUE, full.names=TRUE),
      file.info(dir(path, include.dirs=TRUE, full.names=TRUE))[,c(1:2)], 
      stringsAsFactors=FALSE), isdir==TRUE)
    
    # figure out number of subdirectories
    folders$Subdirs <- apply(folders, 1, function(x) length(which(file.info(dir(x[2], full.names=T))$isdir)))
    
    
    #FIXME: check what is going on here with the subdirectory...
    return(folders[c("Folder", "Subdirs", "Path")])
  }
  
  # check for subfolders
  fileBrowser.hasOffspring <- function(children, user.data=NULL, ...) return(children$Subdirs > 0) # which items have subdirectories
  fileBrowser.icons <- function(children,user.data=NULL, ...) return(rep("gtk-directory", length=nrow(children))) # FIXME: could implement some indicator which folders have already been used
  
  # tree
  tree <- gtree(fileBrowser.items, fileBrowser.hasOffspring, chosencol=3, icon.FUN = fileBrowser.icons, container=mainGrp, expand=TRUE)
  
  # tree click handler
  addHandlerClicked(tree, handler=function(h,...) {
    if (!is.null(val <- svalue(tree)))
      hub$setWD(val)
    else
      hub$setWD(hub$getHome()) # set back to home directory
    showInfo(gui, module, hub$getWD(), timer=NULL, okButton=FALSE)
  })
})

