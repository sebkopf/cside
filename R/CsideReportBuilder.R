#' @include GuiModule.R
NULL

CsideReportBuilder = setRefClass(
  'CsideReportBuilder', contains='GuiModule', 
  fields = list (), 
  methods = list(
    initialize = function(...){
      callSuper(gui = CsideReportBuilderGui(), ...)
      
      # settings
      setSettings(
        windowTitle = "CSIDE - Report Builder",
        launchIcon = "gtk-find-and-replace",
        launchName = "Reports"
      )
    }
  )
)


CsideReportBuilderGui <- setClass("CsideReportBuilderGui", contains="BaseGui")

setMethod("getMenuXML", "CsideReportBuilderGui", function(gui, module) {
  return (
    '<menu name = "CSIDE" action="CSIDE">
    <menuitem action="Help"/>
    <menuitem action="Reload"/>
    <menuitem action="Quit" />
    </menu>')
})

setMethod("getToolbarXML", "CsideReportBuilderGui", function(gui, module) {
  return ('<toolitem action="Hub"/>')
})

setMethod("setNavigationActions", "CsideReportBuilderGui", function(gui, module, actionGrp) {
  nav.actions <-
    list(## name, icon, label , accelerator , tooltip , callback
      list ("CSIDE" , NULL , "_CSIDE" , NULL , NULL , NULL ) ,
      list ("Help" , "gtk-info" ,"Help" , "<ctrl>H" , NULL , function(...) gmessage("sorry, not implemented yet") ) ,
      list ("Reload" , "gtk-refresh" ,"Reload Screen" , "<ctrl>R" , NULL , function(...) remakeGui(gui, module) ) , # FIXME: disable the keyboard shortcut!
      list ("Quit", "gtk-quit", "Quit", "<ctrl>Q", "Quit program", function(...) destroyGui(gui, module) ),
      list ("Hub", "gtk-network", "Start", NULL, "Return to start page", 
            function(...) module$launchModule('start'))
    ) 
  actionGrp$addActions(nav.actions)
})

setMethod("makeMainGui", "CsideReportBuilderGui", function(gui, module) {
  # top level groups
  setMenuGroup(gui, module, ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0))
  setToolbarGroup(gui, module, ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0))
})

