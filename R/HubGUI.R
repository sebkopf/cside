# S4 class HubGUI
HubGUI <- setClass("HubGUI", contains="GUI")

setMethod("getMenuXML", "HubGUI", function(gui, hub) {
  return (
    '<menu name = "CSIDE" action="CSIDE">
      <menuitem action="Help"/>
      <menuitem action="Reload"/>
      <menuitem action="Quit" />
    </menu>')
})

setMethod("getToolbarXML", "HubGUI", function(gui, hub) {
  nav <- '<separator expand="true"/>'
  for (name in names(hub$modules)) # add all modules to toolbar
    if (name!='hub')
      nav <- paste0(nav, '<toolitem action="Module', name,'"/>')
  return (paste0(nav, '<separator expand="true"/>'))
})

setMethod("setNavigationActions", "HubGUI", function(gui, hub, actionGrp) {
  nav.actions <-
    list(## name, icon, label , accelerator , tooltip , callback
      list ("CSIDE" , NULL , "_CSIDE" , NULL , NULL , NULL ) ,
      list ("Help" , "gtk-info" ,"Help" , "<ctrl>H" , NULL , function(...) gmessage("sorry, not implemented yet") ) ,
      list ("Reload" , "gtk-refresh" ,"Reload Screen" , "<ctrl>R" , NULL , function(...) remakeGUI(gui, hub) ) , # FIXME: disable the keyboard shortcut!
      list ("Quit", "gtk-quit", "Quit", "<ctrl>Q", "Quit program", function(...) destroyGUI(gui, hub) ))
  
  # add module launch actions to toolbar
  for (name in names(hub$modules)) {
    if (name!='hub') {
      nav.actions <- c(nav.actions, list(
        list(paste0("Module", name), # action handle
             hub$getModule(name)$getSetting("launchIcon"), # icon
             hub$getModule(name)$getSetting("launchName"), # label
             NULL, # short cut
             hub$getModule(name)$getSetting("launchTooltip"), # tooltip
             function(...) hub$launchModule(name)))) # launch module
    }
  }
  actionGrp$addActions(nav.actions)
})

setMethod("makeMainGUI", "HubGUI", function(gui, hub) {
  # top level groups
  setMenuGroup(gui, hub, ggroup(horizontal=FALSE, cont=getWinGroup(gui, hub), spacing=0))
  mainGrp <- ggroup(horizontal=FALSE, cont=getWinGroup(gui, hub), spacing=0, expand=TRUE)
  infoGrp <- ggroup(horizontal=FALSE, cont=getWinGroup(gui, hub), spacing=0)
  setToolbarGroup(gui, hub, ggroup(horizontal=FALSE, cont=getWinGroup(gui, hub), spacing=0))
  
  # working directory selection
  # file browser
  fileBrowser.items <- function(path = NULL, user.data=NULL) {
    if (is.null(path)) 
      path <- hub$getHome()
    else
      path <- file.path(hub$getHome(), do.call("file.path", args=as.list(path)))
    
    hub$setWD(path)
    showInfo(gui, hub, hub$getWD(), timer=NULL, okButton=FALSE)
  
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
    showInfo(gui, hub, hub$getWD(), timer=NULL, okButton=FALSE)
  })
})

