#' @note This is just the bare bones GUI for the isodat data processor
#' @note todo: migrate the whole github.com/sebkopf/idp implementation
#' over this way to CSIDE

# S4 class DHModuleGUI
DHModuleGUI <- setClass("DHModuleGUI", contains="BaseGui")

setMethod("makeMainGui", "DHModuleGUI", function(gui, module) {
  # top level groups
  navgrp<-ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0) # navigation group
  setMenuGroup(gui, module, navgrp)
  setToolbarGroup(gui, module, navgrp)
  
  # left top
  glt<-ggroup(horizontal=FALSE, expand=TRUE) # left column file browser group
  # left bottom
  glb<-ggroup(horizontal=FALSE, expand=TRUE) # left column for file info
  glbGTK<-getToolkitWidget(glb) # gtk object
  glbGTK['border-width']<-5 # border with
  # right top
  grt<-ggroup(horizontal=TRUE, expand=TRUE) # right top
  plot.grp<-ggroup(horizontal=FALSE, expand=TRUE, container=grt) # plot grp in right top
  # right bottom
  grb<-ggroup(horizontal=TRUE, expand=TRUE) # right bottom
  setWidgets(gui, module, tableGrp = ggroup(horizontal=FALSE, expand=TRUE, container=grb)) # table grp in bottom right
  setWidgets(gui, module, leftGrp = gpanedgroup(glt, glb, horizontal=FALSE, expand=TRUE)) # left column
  setWidgets(gui, module, rightGrp = gpanedgroup(grt, grb, horizontal=FALSE, expand=TRUE)) # right column
  setWidgets(gui, module, mainGrp = gpanedgroup(getWidgets(gui, module, "leftGrp"), getWidgets(gui, module, "rightGrp"), horizontal=TRUE, container=getWinGroup(gui, module), expand=TRUE)) # total window group
  
  ### files browser
  setWidgets(gui, module, 
             fileBrowserParentGrp = getWidgets(gui, module, 'leftGrp'),
             fileBrowserGrp = NULL)
  
#   # all the items of the file browser
#   fileBrowser.items <- function(path = NULL, user.data=NULL) {
#     topleveldir <- is.null(path)
#     if (topleveldir) 
#       path <- tag(getWindow(gui, module), "settings")$fileDirectory # start with file directory from the settings
#     files <- file.info(dir(path=path, full.names=TRUE))[,c(1,2,3)] # get all files and folders
#     files <- data.frame(Name=dir(path=path), Dir=files[,2], stringsAsFactors=FALSE)
#     files <- files[union(which(files$Dir), grep("\\.cf$", files$Name)),] # only folders and .cf files
#     files <- files[order(-files$Dir, files$Name),] # sort by folder vs file and then name
#     if (topleveldir) # add "..." at the beginning (FIXME: could make sure that it's not added if at root but too lazy now)
#       files <- rbind(data.frame(Name="...", Dir=FALSE), files)
#     return(files)
#   }
#   
#   # functions to determine which items have sub items and which icons to use
#   fileBrowser.hasOffspring <- function(children,user.data=NULL, ...)return(children$Dir) # which items have subdirectories
#   fileBrowser.icons <- function(children,user.data=NULL, ...) {
#     x <- rep("gtk-new", length=nrow(children))
#     x[which(children$Name=="...")] <- "gtk-directory" # top level folder
#     x[children$Dir] <- "gtk-directory" # real folders
#     return(x)
#   }
#   
#   # function to remake the file tree when switching folders (FIXME: somehow looses ability for tree expansion...maybe something about how the tree is added?)
#   fileBrowser.gui<-function() {
#     if (!is.null(getWidgets(gui, module, "fileBrowserGrp")))
#       delete(getWidgets(gui, module, "fileBrowserParentGrp"), getWidgets(gui, module, "fileBrowserGrp"))
#     getWidgets(gui, module, "fileBrowserGrp") <- ggroup(expand=TRUE)
#     add(getWidgets(gui, module, "fileBrowserParentGrp"), getWidgets(gui, module, "fileBrowserGrp"), expand=TRUE)
#     tree<-gtree(fileBrowser.items, fileBrowser.hasOffspring, icon.FUN = fileBrowser.icons, container=getWidgets(gui, module, "fileBrowserGrp"), expand=TRUE, handler=function(h,...) {
#       if (is.null(subpath <- svalue(h$obj[]))) { 
#         path <- IDP.getSettings(idp, "fileDirectory")
#         file <- svalue(h$obj)
#       } else {
#         path <- file.path(IDP.getSettings(idp, "fileDirectory"), do.call("file.path", args=as.list(subpath[-length(subpath)])))
#         file <- subpath[length(subpath)]
#       }
#       
#       newpath <- file.path(path, file) # assemble new path  
#       if (identical(file, "...")) { # except: top level directory
#         pathparts <- strsplit(path, .Platform$file.sep)[[1]]
#         newpath <- do.call("file.path", args=as.list(pathparts[-length(pathparts)]))
#       } 
#       
#       if (file.info(newpath)$isdir) { # open directory
#         IDP.setSettings(idp, list(fileDirectory = newpath))
#         fileBrowser.gui()
#       } else # open file
#         IDP.openIsodatFile(idp, path, file)
#     })
#   }
#   # --> file browser initialization is at the end because it needs other idp objects to be fully initialized
  
  ### file info group
  fileInfo<-list()
  fileInfo.nb <- gnotebook(cont = glb, expand=TRUE)
  fileInfo.layout <- glayout(cont = fileInfo.nb, expand=TRUE, label="File Info")
  fileInfo.layout[1, 1] <- "File:"
  fileInfo.layout[1, 2] <- (fileInfo$Filename <- glabel("", cont=fileInfo.layout))
  fileInfo.layout[2, 1] <- "GC:"
  fileInfo.layout[2, 2] <- (fileInfo$GCprogram <- glabel("", cont=fileInfo.layout))
  fileInfo.layout[3, 1] <- "AS:"
  fileInfo.layout[3, 2] <- (fileInfo$ASprogram <- glabel("", cont=fileInfo.layout))
  fileInfo.layout[4, 1] <- "MS:"
  fileInfo.layout[4, 2] <- (fileInfo$MSprogram <- glabel("", cont=fileInfo.layout))
  fileInfo.layout[5, 1] <- "H3:"
  fileInfo.layout[5, 2] <- (fileInfo$H3factor <- glabel("", cont=fileInfo.layout))
  
  # store widgets
  setWidgets(gui, module, 
             fileInfo = fileInfo, 
             fileInfo.nb = fileInfo.nb,
             info.graph = ggraphics(container=fileInfo.nb, label="Refs", expand=TRUE)) # file graph
  
  ### plot grp
  #FIXME
#   setWidgets(gui, module, pn = pn.GUI(plot.grp, getWindow(gui, module), enablePlotLabel=FALSE, enableMenuButtons=FALSE, startWithTab=FALSE,
#                        plotObjLoadHandler=function(obj) IDP.loadIsodatFileTab(idp, obj$plotinfo),
#                        plotEventHandlers=list(
#                          Changed = function(h,...) IDP.plotClickHandler(idp, h),
#                          Doubleclick = function(h,...) IDP.plotDoubleClickHandler(idp, h),
#                          Rightclick = function(h,...) IDP.plotRightClickHandler(idp, h))))
  
  ### table grp
#FIXME  setWidgets(gui, module, dataTable = gtable(IDP.getEmptyPeakTable(idp), expand=TRUE, cont=getWidgets(gui, module, "tableGrp")))
  
#  ### initialize file browser (this late so it has access to the different objects created later)
#  fileBrowser.gui()
  
#   ### make window visible after it's completely initialized
#   optionsSwitch(modeacts, modeact_IDs, list(name=getSettings(gui, module, "mode")) # select right option from the start
#   optionsSwitch(xaxisActs, xaxisSignals, list(name=getSettings(gui, module, "plotOptions")$plotOptions$xUnits$ids[[getSettings(gui, module, "plotOptions")$xUnits$value]])) # select right xaxis from the start
#   
#   visHandler <- addHandlerFocus(getWindow(gui, module), handler=function(...) {
#     ### starting plot and notebook positions
#     visible(getWidgets(gui, module, 'info.graph')) <- TRUE
#     plot.new()
#     text(0, 0, " ")
#     svalue(idp$gui$fileInfo.nb) <- 1
#     addHandlerChanged(idp$gui$fileInfo.nb, handler=function(h,...) if (h$pageno ==2) IDP.plotRefs(idp))
#     
#     ### divider positions
#     svalue(getWidgets(gui, module, "leftGrp")) <- getSettings(gui, module, "leftPane")
#     svalue(getWidgets(gui, module, "rightGrp")) <- getSettings(gui, module, "rightPane")
#     svalue(getWidgets(gui, module, "mainGrp")) <- getSettings(gui, module, "centerPane")
#     
#     # block Handler (only want it to fire once)  
#     blockHandler(getWindow(gui, module), ID=visHandler)
#   })
})

setMethod("setNavigationActions", "DHModuleGUI", function(gui, module, actionGrp) {
  nav.actions <-
    list(## name, icon, label , accelerator , tooltip , callback
      list ("IDP" , NULL , "_IDP" , NULL , NULL , NULL ) ,
      list ("SaveToWorkspace" , "gtk-home" , "Save To Workspace" , "<ctrl>H" ,"Save settings and data to workspace" , function(...) { 
        IDP.showInfo(idp, "Saving to workspace...", timer=1, okButton=FALSE)
        IDP.save(idp)
        IDP.showInfo(idp, "Isodat File Processor settings and data succesfully saved to workspace.", timer=2, okButton=FALSE)
      } ) , 
      list ("Quit", "gtk-quit", "Quit", NULL, "Quit program", function(...) { if (gconfirm("Are you sure you want to quit?")) destroyGui(gui, module) } ),
      list ("File" , NULL , "_File" , NULL , NULL , NULL ) , 
      list ("OpenFile" , "gtk-open" , "Open File" , "<ctrl>O" ,"Open isodat file" , function(...) message("sorry, file selection dialog not implemented yet") ) , 
      list ("CloseFile" , "gtk-close" , "Close File" , "<ctrl>W" ,"Close isodat file" , getWidgets(gui, module, "pn")$actions$aClosePlot$handler ) , 
      list ("CloseAll" , "gtk-stop" , "Close all Files" , "<ctrl><shift>W" ,"Close all isodat files" , function(...) gmessage("sorry, not implemented yet") ) ,
      list ("ExportExcel" , "gtk-save-as" , "Export to Excel" , "<ctrl>X" , "Export raw data and peak table to excel" , function(...) idp.exportFileToExcel(idp) ) , 
      list ("ExportAll" , "gtk-harddisk" , "Export all to Excel" , "<ctrl><shift>X" , "Export peak table for all files" , function(...) idp.exportAllPeakTablesToExcel(idp, saveAll=TRUE) ) , 
      list ("Plot" , NULL , "_Chrom" , NULL , NULL , NULL ) , 
      list ("SavePlot", "gtk-save-as", "Save as PDF", "<ctrl>S", "Save chromatogram as PDF", getWidgets(gui, module, "pn")$actions$aSavePlot$handler ),
      list ("SaveAll", "gtk-harddisk", "Save all", "<ctrl><shift>S", "Save all chromatograms as PDFs", getWidgets(gui, module, "pn")$actions$aSaveAll$handler ),
      list ("PrintPlot", "gtk-print", "Print", NULL, "Print chromatogram", getWidgets(gui, module, "pn")$actions$aPrintPlot$handler ),
      list ("Help" , "gtk-info" ,"Help" , NULL , NULL , function(...) gmessage("sorry, not implemented yet") ) ,
      list ("View" , NULL , "_View" , NULL , NULL , NULL ) ,
      list ("ZoomFull", "gtk-zoom-fit", "Unzoom", NULL, "Unzoom", function(h,...) IDP.zoomReset(idp) ),
      list ("ZoomBack", "gtk-undo", "Undo Zoom", "<ctrl>Z", "Return to previous zoom", function(h,...) IDP.zoomBack(idp) ),
      list ("ZoomIn", "gtk-zoom-in", "Zoom in", "<ctrl>P", "Zoom in", function(h,...) IDP.zoomIn(idp) ), # fix me, bertter short cuts?
      list ("ZoomOut", "gtk-zoom-out", "Zoom out", "<ctrl>M", "Zoom out", function(h,...) IDP.zoomOut(idp) ), # fix me, better short cuts?
      list ("MoveIntervalB", "gtk-go-back", "Move Left", "<ctrl><shift>Left", "Move visible window to the left", function (h,...) IDP.zoomMove(idp, -1)), # FIXME: shortcuts don't work
      list ("MoveIntervalF", "gtk-go-forward", "Move Right", "Right", "Move visible window to the right", function (h,...) IDP.zoomMove(idp, +1)), # FIXME: shortcuts don't work
      list ("SetAxes", "gtk-page-setup", "Set Axes", NULL, "Set axis coordinates", function(h,...) print("set axes") ), # fix me, better short cuts?
      list ("DataTable" , NULL , "_Data" , NULL , NULL , NULL ) ,
      list ("Mode" , NULL , "_Mode" , NULL , NULL , NULL ) ,
      list ("Settings" , NULL , "_Settings" , NULL , NULL , NULL ) , 
      list ("XUnits" , NULL , "_X-axis Units" , NULL , NULL , NULL ) ,
      list ("PlotOptions" , NULL , "_Plot Options" , NULL , NULL , NULL ) ,
      list ("EditSettings" , "gtk-preferences" , "_Edit Settings" , NULL , NULL , function(...) {} ) , 
      list ("TableColumns", "gtk-properties", "_Table Columns", NULL, "Select which peak table columns are displayed", function(...) {
        dlg <- gbasicdialog(title="Select visible columns for the peak table", handler = function(h,...) {
          tag(getWindow(gui, module), "settings")$peakTableColumns <- tbl[] # save updated peak table column settings
          delete(getWidgets(gui, module, "tableGrp"), getWidgets(gui, module, "dataTable")) # delete previous table
#FIXME          setWidgets(gui, module, dataTable = gtable(IDP.getEmptyPeakTable(idp), expand=TRUE, cont=getWindow(gui, module, "tableGrp"))) # remake current table
          IDP.loadPeakTable(idp, pn.getAllInfo(getWidgets(gui, module, "pn"))$peakTable)
        })
        size(dlg)<-c(500,500)
        tbl <- table.toggleTable(ggroup(cont=dlg, expand=TRUE), tag(getWindow(gui, module), "settings")$peakTableColumns, "Show", invisibleColumns=c("Name", "Required", "IsodatCol"))
        visible(dlg, set=TRUE) ## show dialog
      }),
      list ("DeletePeak" , "gtk-cancel" , "Delete Peak", "<ctrl>D", "Delete selected peak (<ctrl>D)" , function(...) {gmessage("sorry, not implemented yet")}),
      list ("CopyTable" , "gtk-copy" , "Copy Table", "<ctrl>C", "Copy the peak table to the clipboard." , function(...) IDP.copyPeakTable(idp)),
      list ("Recalculate" , "gtk-execute" , "Re-evaluate", "<ctrl>R", "Recalculate the isotopic composition based on the standards picked." , function(...) IDP.recalculatePeakTable(idp)),
      list ("Revert" , "gtk-revert-to-saved" , "Discard All", NULL, "Discard all changes and return to original peak table from data file." , function(...) IDP.revertPeakTable(idp))
    )

  actionGrp$addActions( nav.actions )
  
  ### special actions
  # full screen (toggle)
  fullscreen_act<-gtkToggleAction("FullScreen", "Full Screen", "Make application full screen", stock.id="gtk-fullscreen")
  gSignalConnect (fullscreen_act , "toggled" , function ( action ) {
    if(fullscreen_act ['active'] )
      getToolkitWidget(getWindow(gui, module))$fullscreen ( )
    else
      getToolkitWidget(getWindow(gui, module))$unfullscreen ( )
  } )
  actionGrp$addActionWithAccel(fullscreen_act, "<control>F")
  
  # switch between multiple options [generic implementation]
  optionsSwitch<-function(options, signals, action) {
    for (name in names(options)) {
      gSignalHandlerBlock(options[[name]], signals[[name]])
      options[[name]]['active'] <- identical(name, action$name)
      gSignalHandlerUnblock(options[[name]], signals[[name]])
    }
    return(action$name)
  }
  
  # switching x axis unit
  xaxisActs<-list(
    XaxisSec = gtkToggleAction("XaxisSec", "Seconds", "Show graph in seconds"),
    XaxisMin = gtkToggleAction("XaxisMin", "Minutes", "Show graph in minuts")
  )
  xaxisSignals<-list()
  for (name in names(xaxisActs)) {
    xaxisSignals[[name]] <- gSignalConnect (xaxisActs[[name]] , "toggled", function(action) {
      opt <- optionsSwitch(xaxisActs, xaxisSignals, action)
#FIXME      setSetting(tag(getWindow(gui, module), "settings")$plotOptions$xUnits$value <- which(idp$settings$plotOptions$xUnits$ids == opt)
#FIXME      IDP.plot(idp) # replot
    })
    actionGrp$addAction(xaxisActs[[name]])
  }
  
  # plot options
  MarkRefsAct = gtkToggleAction("MarkRefs", "Mark Reference Peaks", "Mark reference peaks with *")
  MarkRefsAct['active']<-getSettings(gui, module, "plotOptions")$markRefs
  gSignalConnect (MarkRefsAct, "toggled" , function ( action ) {
    #tag(getWindow(gui, module), "settings")$plotOptions$markRefs <- MarkRefsAct['active']
#FIXME    IDP.setSettings(idp, list("plotOptions.markRefs" = MarkRefsAct['active']))
#FIXME    IDP.plot(idp) # replot
  } )
  actionGrp$addAction(MarkRefsAct)
  
  # best fit (toggle)
  setWidgets(gui, module, bestfitActive = gtkToggleAction("BestFit", "Best Fit", "Fit to tallest peak", stock.id="gtk-zoom-100"))
  gSignalConnect (getWidgets(gui, module, "bestfitActive") , "toggled" , function ( action ) {
    if (getWidgets(gui, module, "bestfitActive")['active'])
      IDP.zoomBest(idp)
  } )
  actionGrp$addActionWithAccel(getWidgets(gui, module, "bestfitActive"), "<control>B")
  
  # switch between modes
  modeacts<-list(
    ModeInfo = gtkToggleAction("ModeInfo", "Information", "Select peaks for information", stock.id="gtk-leave-fullscreen"),
    ModeAdd = gtkToggleAction("ModeAdd", "Add Peak", "Add peaks", stock.id="gtk-add"),
    ModeEdit = gtkToggleAction("ModeEdit", "Edit Peak", "Edit peaks", stock.id="gtk-redo"),
    ModeRefs = gtkToggleAction("ModeRefs", "Choose Refs", "Choose isotopic reference peaks", stock.id="gtk-about"))
  modeact_IDs<-list()
#FIXME  for (name in names(modeacts))
#FIXME    modeact_IDs[[name]] <- gSignalConnect (modeacts[[name]] , "toggled" , function(action) tag(getWindow(gui, module), "settings")$mode<-optionsSwitch(modeacts, modeact_IDs, action))
  actionGrp$addActionWithAccel(modeacts$ModeInfo, "<control>I")
  actionGrp$addActionWithAccel(modeacts$ModeAdd, "<control>A")
  actionGrp$addActionWithAccel(modeacts$ModeEdit, "<control>E")
  actionGrp$addActionWithAccel(modeacts$ModeRefs, NULL) #FIXME (control - C taken by copy peak table)
})

setMethod("getToolbarXML", "DHModuleGUI", function(gui, module) {
  nav <- '
    <toolitem action = "ZoomFull"/>
    <toolitem action = "ZoomIn"/>
    <toolitem action = "ZoomOut"/>
    <toolitem action = "BestFit"/>
    <toolitem action = "MoveIntervalB"/>
    <toolitem action = "MoveIntervalF"/>
    <toolitem action ="SetAxes"/>
    <separator/>
    <toolitem action ="ModeInfo"/>
    <toolitem action ="ModeAdd"/>
    <toolitem action ="ModeEdit"/>
    <toolitem action ="DeletePeak"/>
    <toolitem action ="ModeRefs"/>
    <separator/>
    <toolitem action ="CopyTable"/>
    <toolitem action ="Recalculate"/>
    <toolitem action ="Revert"/>'
  return(nav)
})

setMethod("getMenuXML", "DHModuleGUI", function(gui, module) {
  return (
    '<menu name = "IDP" action="IDP">
    <menuitem action="SaveToWorkspace" />
    <menuitem action="Help"/>
    <menuitem action="Quit" />
    </menu>
    <menu name ="FileMenu" action ="File">
    <menuitem action ="OpenFile" />
    <menuitem action ="CloseFile" />
    <menuitem action ="CloseAll" />
    </menu>
    <menu name="View" action="View">
    <menuitem action="ZoomBack"/>
    <menuitem action="ZoomFull"/>
    <menuitem action="ZoomIn"/>
    <menuitem action="ZoomOut"/>
    <menuitem action="BestFit"/>
    <menuitem action="MoveIntervalB"/>
    <menuitem action="MoveIntervalF"/>
    <menuitem action="SetAxes"/>
    <separator/>
    <menuitem action="FullScreen"/>
    </menu>
    <menu name="PlotMenu" action="Plot">
    <menu name="XUnits" action="XUnits">
    <menuitem action="XaxisSec"/>
    <menuitem action="XaxisMin"/>
    </menu>
    <menu name="PlotOptions" action="PlotOptions">
    <menuitem action="MarkRefs"/>
    </menu>
    <separator/>
    <menu name="Mode" action="Mode">
    <menuitem action ="ModeInfo"/>
    <menuitem action ="ModeAdd"/>
    <menuitem action ="ModeEdit"/>
    <menuitem action ="ModeRefs"/>
    </menu>
    <menuitem action ="DeletePeak"/>
    <separator/>
    <menuitem action="SavePlot"/>
    <menuitem action="SaveAll"/>
    <separator/>
    <menuitem action="PrintPlot"/>
    </menu>
    <menu name="DataTable" action="DataTable">
    <menuitem action ="CopyTable"/>
    <menuitem action="TableColumns"/>
    <separator/>
    <menuitem action ="ExportExcel" />
    <menuitem action ="ExportAll" />
    </menu>
    <menu name="Settings" action="Settings">  
    <menuitem action="EditSettings"/>
    </menu>')
})
