##################################
# code for generic GUI functions #
# Copyright 2013 Sebastian Kopf  #
# seb.kopf@gmail.com             #
##################################

###########
# WINDOWS #
###########

# standard window
# -- makes gwindow or gdialogbasic depending on input
win.init <- function (title = "", width = 100, height = 100, modal = FALSE, parent=NULL) {
  # get window going
  if (modal) {
    gw <- gbasicdialog(title=title, do.buttons=FALSE, parent=parent) 
    size(gw) <- c(width, height)
  } else
    gw <- gwindow(title, visible=FALSE, width=width, height=height)
  return (gw)
}


##########
# COMBOS #
##########

# load combo
# sort=FALSE/TRUE does alphabetical sorting
combo.load<-function(combo, entries, sort=FALSE, selection=NULL, blockHandlers=NULL) {
  combo.blockHandlers(combo, blockHandlers)
  if (sort) # sort the combo
    entries<-entries[order(entries)]
  combo[]<-entries
  if (!is.null(selection))
    combo.setSelection(combo, selection)
  combo.unblockHandlers(combo, blockHandlers)
}

# add to combo
combo.add<-function(combo, entry, sort=FALSE, select=TRUE, blockHandlers=NULL) {
  if (select)
    selection<-entry # select new entry
  else
    selection<-1 # select first entry
  combo.load(combo, c(combo[], entry), sort=sort, selection=selection, blockHandlers=blockHandlers)
}

# delete selected entry
combo.deleteSelection<-function(combo, reSelect=TRUE, blockHandlers=NULL) {
  index<-combo.getSelection(combo, index=TRUE)
  if (reSelect) 
    selection<-min(index,length(combo[])-1) # select the second to last object if the delted one was the last one
  else
    selection<-1 # select first entry otherwise
  combo.load(combo, combo[-index], selection=selection, blockHandlers=blockHandlers)
}

# selection
combo.getSelection<-function(combo, index=FALSE) return (svalue(combo, index=index))
combo.setSelection<-function(combo, value) {
  if (is.numeric(value))
    svalue(combo, index=TRUE)<-value
  else
    svalue(combo, index=FALSE)<-value
}

# block handlers
combo.blockHandlers<-function(combo, handlerIDs){
  if (!is.null(handlerIDs))
    for (handlerID in handlerIDs)
      blockHandler(combo, handlerID)
}

# unblock handlers
combo.unblockHandlers<-function(combo, handlerIDs){
  if (!is.null(handlerIDs))
    for (handlerID in handlerIDs)
      unblockHandler(combo, handlerID)
}

##########
# TABLES #
##########

####### ENTRIES (get, set, add, edit, delete) ########

# get value of a table entry at the provided index
# if no field is given, returns whole row, if field is given (e.g. c("adsfs", "sdfd")) then only field
table.getValue<-function(table, index, fields=NULL) {
  if (!is.null(fields))
    return(table[][index, fields])
  else
    return(table[][index, ])
}

# get the selected index of a table
table.getSelectedIndex<-function(table) {
  if (!is.empty(index<-svalue(table, index=TRUE)))
    return (index)
  else
    return (NULL)
}

# get selected value from a table (depends on what is defined as the selection column, usually the first column)
# or if providing field param, get specific one
table.getSelectedValue<-function(table, field=NULL) {
  if (is.null(field) && !is.empty(value<-svalue(table, index=FALSE)))
    return (value)
  else if (!is.null(field) && !is.null(dataset<-table.getSelection(table)))
    return (dataset[field])
  return (NULL)
}

# get selected record from a gtable
table.getSelection<-function(table) {
  if (!is.null(index<-table.getSelectedIndex(table))) { # check if anything is selected
    if (is.null(nrow(table[]))) 
      return(table[]) # if there's only one object in the table
    else
      return(table[][index, ]) # if there's multiple objects, return just selected
  }
  return (NULL)
}

# select something in the table
# blocks the changed handler if provided during the selection
table.setSelectedValue<-function(table, value, index=FALSE, blockHandlers=NULL) {
  table.blockHandlers(table, handlers=blockHandlers) # block handlers
  svalue(table, index=index)<-value # set value (use index to decide whether by value or by index)
  table.unblockHandlers(table, handlers=blockHandlers) # unblock handlers
}

# delete selected entry in a data table
# reselect - reselects the record at the same position
# if blockChangedHandlerID is provided, it will block this handler before doing the reselection business (and then unblock it)
table.deleteSelection<-function(table, reSelect=FALSE, blockHandlers=NULL) {
  if (!is.null(index<-table.getSelectedIndex(table))) { # check if anything is selected
    if (!is.null(total<-nrow(table[]))) { # does not support deleting the last one
      table.blockHandlers(table, handlers=blockHandlers) # block handlers
      table[]<-table[][-index,]  # remove line
      if (reSelect) {
        if (index==total) index<-max(1,total-1) # select the second to last object if the delted one was the last one
        table.setSelectedValue(table, index, index=TRUE) 
      }
      table.unblockHandlers(table, handlers=blockHandlers)
    } else
      print("WARNING: can't delete the last item in a table")
  }
}

# update table entry
table.update<-function(table, data, index, ignoreMissing = FALSE) {
  for (field in names(data)) {
    if (field%in%names(table[])) # update table field
      table[][index,field]<-data[[field]]
    else if (!ignoreMissing)
      cat(paste("\nWARNING (to disable, pass ignoreMissing = TRUE):\n\tTrying to update table field", 
                  field, #"with value", data[[field]], 
                  "but field does not exist in table (", paste(names(table[]), collapse=", "), ")"))  
  }
}

# update currently selected table entry
table.updateSelection<-function(table, data, ignoreMissing = FALSE) {
  if (!is.null(index<-table.getSelectedIndex(table))) # check if anything is selected
      table.update(table, data, index, ignoreMissing = ignoreMissing)
}

# add an entry in the data table
# index --> add at specific index, if NULL, adds at the end of the table (or if index > length of table)
# returns index of newly added entry
table.add<-function(table, data, index=NULL, select=TRUE, blockHandlers=NULL, ignoreMissing = FALSE) {
  table.blockHandlers(table, handlers=blockHandlers) # block handlers
  if (!is.null(index) && index<=nrow(table[])) 
    table[][(index+1):(nrow(table[])+1),]<-table[][index:nrow(table[]),] # move records up one
  else
    index<-nrow(table[])+1 # adding at the end of the table
  table.update(table, data, index, ignoreMissing = ignoreMissing)
  
  if (select) 
    table.setSelectedValue(table, index, index=TRUE) 
  table.unblockHandlers(table, handlers=blockHandlers) # unblock handlers
  return (index)
}

# add an entry in the data table after the current selection
# if nothing is selected, adds a new entry at the end of the table
# returns index of newly added entry
table.addAfterSelection<-function(table, data, select=TRUE, blockHandlers=NULL, ignoreMissing = FALSE) {
  if ( !is.null(index<-table.getSelectedIndex(table)))
    index<-index + 1
  return (table.add(table, data, index=index, select=select, blockHandlers=blockHandlers, ignoreMissing = ignoreMissing))
}

####### HANDLERS ##########

# table handler blocks (This is only implemented this way because handler blocking does not seem to work for gtables!)
# important, need to implement this in the handlers though!!
table.blockHandlers<-function(obj, handlers=c("changed", "clicked")) for (handler in handlers) tag(obj, paste(handler,"Handler", sep=""))<-TRUE
table.unblockHandlers<-function(obj, handlers=c("changed", "clicked")) for (handler in handlers) tag(obj, paste(handler,"Handler", sep=""))<-FALSE
table.isHandlerBlocked<-function(obj, handler=NULL) {
  if (is.null(handler) || is.null(tag(obj, paste(handler,"Handler", sep=""))))
      return (FALSE)
  return (tag(obj, paste(handler,"Handler", sep="")))
}
  
###### TABLE SORTING ########

# datasets move up and down in tables
# move = c("up", "down", "top", "bottom"), pick one
# gvar = the name of a global variable to update
# tableID = if a global varaible is provided, use this field in the table to find the right entries in the global variable to update
# gvarID = if a global variable is provided, use this field to find the right entries to update
# gvarOrder = if a global variable is provided, use this field to keep track of the locations
table.moveHandler<-function(table, move=NULL, gvar=NULL, tableID="ID", gvarID="ID", gvarOrder="Order", blockHandlers=NULL) {
  if (!is.null(move)) { # only move if a move command is provided
    if (!is.empty(index<-svalue(table, index=TRUE))) { # make sure something is selected
      # block event handlers
      table.blockHandlers(table, handlers=blockHandlers) 
      
      # get new index
      newIndex<-switch(move, "up"=(index-1), "down"=(index+1), "top"=1, "bottom"=nrow(table[])) # find the record to exchange with
      if ( index!=newIndex && newIndex >= 1 && newIndex <= nrow(table[])) { # can move (no point moving outside the table or staying at the same place)
        
        # update global variable if provided
        if (!is.null(gvar)) {
          globalvar<-get(gvar, envir=.GlobalEnv) # get global variable
          
          ID<-table[index, tableID] # figure out which record in the global variable to update
          sort<-globalvar[[gvarOrder]][which(globalvar[[gvarID]]==ID)] # current sort 
          newID<-table[][newIndex, tableID] # figure out ID of new position
          newSort<-globalvar[[gvarOrder]][which(globalvar[[gvarID]]==newID)] # sort of new position
          
          if (move=="top")
            globalvar<-var.updateSorting(globalvar, newSort, sort, orderField=gvarOrder, changeBy=1) # update sorts
          else if (move=="bottom")
            globalvar<-var.updateSorting(globalvar, sort, newSort, changeBy=-1) # update sorts
          else
            globalvar[[gvarOrder]][which(globalvar[[gvarID]]==newID)]<-sort # update exchange item with current sort
          globalvar[[gvarOrder]][which(globalvar[[gvarID]]==ID)]<-newSort
          
          assign(gvar, globalvar, envir=.GlobalEnv) # assign global variable
        }
        
        ### UPDATE TABLE ###
        selectedRow<-table[][index,]
        if (move=="top")
          table[][2:index,]<-table[][1:(index-1),] # move everything down 1 that's above the item
        else if (move=="bottom")
          table[][index:(nrow(table[])-1),]<-table[][(index+1):nrow(table[]),] # move everything up 1 that's below the item
        else # up and down moves
          table[][index,]<-table[newIndex,] # exchange it with the exchange index
        
        # update new index position and select it
        table[][newIndex,]<-selectedRow
        svalue(table, index=TRUE)<-newIndex 
      }
      # unblock event handlers
      table.unblockHandlers(table, handlers=blockHandlers)
    }
  }
}

# provide the buttons for moving data in a table
table.moveButtons<-function(container, table, gvar=NULL, tableID="ID", gvarID="ID", gvarOrder="Order", blockHandlers=c("changed", "clicked") ) {
  moveButtonsGrp<-ggroup(horizontal=FALSE, cont=container)
  addSpring(moveButtonsGrp)
  gbutton(action=gaction("T", icon="gtk-goto-top", handler=function(h,...) table.moveHandler(table, move="top", gvar=gvar, tableID=tableID, gvarID=gvarID, gvarOrder=gvarOrder, blockHandlers=blockHandlers)), cont=moveButtonsGrp) 
  gbutton(action=gaction("U", icon="gtk-go-up", handler=function(h,...) table.moveHandler(table, move="up", gvar=gvar, tableID=tableID, gvarID=gvarID, gvarOrder=gvarOrder, blockHandlers=blockHandlers)), cont=moveButtonsGrp) 
  gbutton(action=gaction("D", icon="gtk-go-down", handler=function(h,...) table.moveHandler(table, move="down", gvar=gvar, tableID=tableID, gvarID=gvarID, gvarOrder=gvarOrder, blockHandlers=blockHandlers)),  cont=moveButtonsGrp)
  gbutton(action=gaction("B", icon="gtk-goto-bottom", handler=function(h,...) table.moveHandler(table, move="bottom", gvar=gvar, tableID=tableID, gvarID=gvarID, gvarOrder=gvarOrder, blockHandlers=blockHandlers)),  cont=moveButtonsGrp)
  addSpring(moveButtonsGrp)
}

####### special tables ########
# table that has toggle abalitiy
# returns a rGtkDataFrame whose data frame content can be accessed by []
table.toggleTable <- function(cont, df, toggleColumn, toggleFunc = NULL, invisibleColumns = NULL) {
  index <- which(names(df) == toggleColumn) # index of toggle column
  model <- rGtkDataFrame(df) # data frame model
  view <- gtkTreeView() # display structure
  
  # visible columns
  if (!is.null(invisibleColumns))
    excludeIndices <- c(index, which(names(df) %in% invisibleColumns))
  else 
    excludeIndices <- index
    
  # cell renderers
  cr <- gtkCellRendererToggle() # cell renderer for the check box
  cr['activatable'] <- TRUE
  
  # add cell renderers to table
  view$insertColumnWithAttributes (0, toggleColumn, cr, active = (index-1) ) 
  mapply(view$insertColumnWithAttributes, -1, colnames(df[-excludeIndices]), list(gtkCellRendererText()), text = (seq_along(df)[-excludeIndices]) -1)
  
  # signal for processing cell renderer
  gSignalConnect (cr, "toggled", function (cr, path, user.data ) {
    row <- (as.numeric(path) + 1)
    model <- user.data$getModel()
    #if (is.null(toggleFunc) || do.call(toggleFunc, list(data = model, row = row))) #FIXME --> write toggle func
    model[row, index] <- !model[row, index]
  }, data=view)
  
  # add model
  view$setModel(model)
  
  # add to a scroll window in container
  scrolled_window <- gtkScrolledWindow()
  getToolkitWidget(cont)$packStart(scrolled_window , expand = TRUE, fill = TRUE)
  scrolled_window$add(view)
  scrolled_window$setPolicy ("automatic", "automatic")
  
  # return model table --> can access data by model[]
  return(model)
}


###########
# GENERIC #
# WIDGETS #
###########

# load widgets from a data object
widgets.load<-function(widgets, data) {
  sapply(names(widgets), function(i) {
    if (i%in%names(data)) { #field exists in data
      if (class(widgets[[i]])[[1]]=="gTable") { # gtable style widgets
        if (class(data[[i]])=="list") # single record
          widgets[[i]][] <- data.frame(data[[i]], stringsAsFactors=FALSE)	
        else # multiple records
          widgets[[i]][] <- data[[i]]	
      } else # all other widgets
        svalue(widgets[[i]]) <- data[[i]]
    } else
      print(paste("WARNING: trying to load widget", i, "but no corresponding field found in dataset."))
  })
}

# get widgets into list
widgets.getValues<-function(widgets) {
  return (sapply(names(widgets), function(var) {
      tryCatch(list(widget.getValue(widgets[[var]])),
               warning=function(w) { return(NA) } ) # coerce with NA values      
    }))
}

# get widget value (returns gTable always as data frame)
widget.getValue<-function(widget) {
  if (class(widget)[[1]]=="gTable") #in case it's a gTable(i.e. data frame), have to access info slightly differently
    if (class(widget[])=="list") # single record is returned as a list rather than a dataframe
      return (data.frame(widget[], stringsAsFactors=FALSE))
    else # multiple records in table are returned properly as data frame
      return (widget[]) 
  else
    return(svalue(widget))
}

# get widgets into dataframe (WARNING: will collapse any widgets that are tables down to NULL, if you want to preserve those, use widgets.getValues instead to get a list)
widgets.getValuesAsDF<-function(widgets) {
  return (data.frame(sapply(names(widgets), function(var) {list(svalue(widgets[[var]]))}), stringsAsFactors=TRUE))
}

#################
# PLOT NOTBEOOK #
#################

#FOR TESTING PURPOSES
#win<-gwindow("blub")
#pn.GUI(gframe(cont=win, horizontal=FALSE), win)

# make GUI forplot notbook
# new plot objs = list() object defining what kind of parameters are on a plot object by default
# - the load handlers are just passed the currently selected plot object for doing whatever they want with it
# add event handlers to the plot as needed, currently supported: "droptarget", "Clicked", "Rightclick", "MouseMotion"
# --> pass like this plotEventHandlers=list(droptarget=fun, Clicked=fun)
pn.GUI<-function(container, window, newPlotObj=NULL, 
                 newPlotObjLoadHandler=NULL, plotObjLoadHandler=NULL, plotEventHandlers=list(),
                 enablePlotLabel=TRUE, enableMenuButtons=TRUE, startWithTab=TRUE){
  
  pn<-list() # plots notebook object
  pn$win<-window
  pn$enablePlotLabel<-enablePlotLabel
  
  # actions to interact with the plots
  #FIXME: figure out how to make keyboard accelerators work (should be key.accel="Control-n" and parent=win for gaction but always fails, not sure why)
  #NOTE: as of august 2013, the keyboard accelerators were not implemented for RGtk2
  pn$actions<-list(
    aNewPlot = list(label="New Plot", icon="gtk-page-setup", handler=function(...) pn.newPlotTab(pn, tabObj=newPlotObj, eventHandlers=plotEventHandlers, loadHandler=newPlotObjLoadHandler, label=paste("Plot", length(pn$plot.nb)+1, sep="")) ),
    aClosePlot = list(label="Close Plot", icon="gtk-cancel", handler=function(...) pn.deletePlotTab(pn, loadHandler=plotObjLoadHandler)), 
    aSavePlot = list(label="Save Plot", icon="gtk-save-as", handler=function(...) pn.savePlotGUI(pn, index=svalue(pn$plot.nb))), 
    aPrintPlot = list(label="Print Plot", icon="gtk-print", handler=function(...) pn.printPlot(pn, index=svalue(pn$plot.nb))), 
    aSaveAll = list(label="Save All", icon="gtk-harddisk", handler=function(...) pn.savePlotGUI(pn)))
  if (enableMenuButtons) {
    pn$buttons.grp<-ggroup(cont=container, horizontal=TRUE)
    addSpring(pn$buttons.grp)
    for (act in pn$actions)
      gbutton(action=gaction(act$label, icon=act$icon, handler=act$handler), cont=pn$buttons.grp)
  }
  
  # plots notebook
  pn$plot.nb <- gnotebook(cont=container, expand=TRUE)
  pn$plot.nb.changedHandler<-addHandlerChanged(pn$plot.nb, handler=function(h,...) pn.selectPlotTab(pn, h$pageno, loadHandler=plotObjLoadHandler))
  if (startWithTab)
    pn.newPlotTab(pn, tabObj=newPlotObj, label="Plot1", loadHandler=newPlotObjLoadHandler, eventHandlers=plotEventHandlers)
  
  return(pn)
}

# save handler
# save the plot with the provided index
# if none is provided, save all plots
pn.savePlotGUI<-function(pn, index=NULL){
  if (is.null(index)) { # save all plots
    f=gfile("Select the folder where to save all the plots.", type="selectdir", cont=pn$win)
  } else { # save index plot
    f=gfile("Select where to save this graph.", type="save", cont=pn$win, 
          initialfilename = paste(format(Sys.time(),format="%Y%m%d"),"_", names(pn$plot.nb)[index],".pdf", sep=""),
          filter = list("PDF Files" = list(patterns=c("*.pdf")), "All files" = list(patterns = c("*"))))
  }
  
  if (!is.na(f)){
    grp<-ggroup(cont=(w<-gwindow("Save plot as pdf", width=200, height=100, spacing=30)), horizontal=FALSE, expand=TRUE)
    dlggrp<-glayout(container=grp, spacing=10)
    dlggrp[1,1]<-glabel("Width [inches]:",con=dlggrp)
    dlggrp[1,2]<-(width <- gedit(8,container=dlggrp, coerce.with=as.numeric))
    
    dlggrp[2,1]<-glabel("Height [inches]:",con=dlggrp)
    dlggrp[2,2]<-(height <- gedit(6,container=dlggrp, coerce.with=as.numeric))
    
    #dlggrp[3,1]<-glabel("Unit:",con=dlggrp)
    #dlggrp[3,2]<-(units <- gcombobox(c("in","cm","mm"),container=dlggrp))
    
    gbutton("save", cont=grp, handler=function(h,...) {
      if (is.null(index)) { # save all
        for (i in 1:length(pn$plot.nb)) 
          pn.savePlot(pn, i, file.path(f, paste(format(Sys.time(),format="%Y%m%d"),"_", names(pn$plot.nb)[i],".pdf", sep="")), width=svalue(width), height=svalue(height))
      } else { # save just the current
        if (length(grep("\\.pdf$", f))==0) f<-paste(f,".pdf",sep="") # ensure .pdf ending
        pn.savePlot(pn, index, f, width=svalue(width), height=svalue(height))
      }
      pn.reactivatePlot(pn) # reactivate previously active plot
      dispose(w)
    })
  }
}

# save the plot with the given index
pn.savePlot<-function(pn, index, file, width=8, height=6) {
  pn.activatePlot(pn, index)
  dev.copy2pdf(file=file, width=width, height=height) # copy graph
}

# print the plot with the given index
pn.printPlot<-function(pn, index, width=8, height=6) {
  if (exists("win.print")) { # on windows, go print
    pn.activatePlot(pn, index)
    win.print(width=width, height=height) # launches print interface
    pn.activatePlot(pn, index) # reactivate graphics device
  } else 
    gmessage("Sorry, direct printing is not yet supported on Linux/MacOS.\nPlease save the plot as a pdf and print from there.")
}

# make new plot tab
# provide more detailed plot object if keeping other parametrs is desired
# add event handlers to the plot as needed, currently supported: "droptarget", "Clicked", "Changed", "Rightclick", "MouseMotion", "RightlickMousePopupmenu" 
# --> pass like this plotEventHandlers=list(droptarget=fun, clicked=fun)
pn.newPlotTab<-function(pn, tabObj=NULL, label="Plot", loadHandler=NULL, eventHandlers=list()) {
  # block handlers
  blockHandler(pn$plot.nb, pn$plot.nb.changedHandler)
  
  # make new tab
  grp<-ggroup(cont=pn$plot.nb, horizontal=FALSE, label=label)
  if (pn$enablePlotLabel)
    addHandlerKeystroke(gedit(label, cont=grp), handler=function(h,...) {pn.changePlotTabName(pn, svalue(h$obj))})
  gg<-ggraphics(cont=grp)
  blockHandler(obj=gg) # disable automatic 2nd mouse button popup handler (for save and copy)
   
  # event handlers
  if (!is.null(eventHandlers$droptarget))
    adddroptarget(gg, targetType="object", handler=eventHandlers$droptarget)
  if (!is.null(eventHandlers$Clicked))
    addHandlerClicked(gg, handler=eventHandlers$Clicked)
  if (!is.null(eventHandlers$Changed))
    addHandlerChanged(gg, handler=eventHandlers$Changed)
  if (!is.null(eventHandlers$Rightclick))
    addHandlerRightclick(gg, handler=eventHandlers$Rightclick)
  if (!is.null(eventHandlers$MouseMotion))
    addHandlerMouseMotion(gg, handler=eventHandlers$MouseMotion)
  if (!is.null(eventHandlers$RightlickMousePopupmenu))
    add3rdMousePopupmenu(obj=gg, menulist=eventHandlers$RightlickMousePopupmenu)
  
  # make new object
  if (is.null(tabObj))
    tabObj<-list() # new object
  tabObj$gg<-gg # store the graphics object
  
  if (length(pn$plot.nb) == 1) 
    tag(pn$plot.nb, "tabs")<-list()
  tag(pn$plot.nb, "tabs")[[length(pn$plot.nb)]]<-tabObj # add new object
  
  # load
  if (!is.null(loadHandler))
    do.call(loadHandler, list(obj=tabObj))
  
  # unblock handlers
  unblockHandler(pn$plot.nb, pn$plot.nb.changedHandler)
}

# change plot tab name (not stored in object)
pn.changePlotTabName<-function(pn, label) {
  names(pn$plot.nb)[svalue(pn$plot.nb)]<-label
}

# delete plot
# (deletes specific plot if index is passed in otherwise just the currently selected ones)
pn.deletePlotTab<-function(pn, index=NULL, loadHandler=NULL) {
  if (!is.null(index))
    svalue(pn$plot.nb)<-index
  else
    index<-svalue(pn$plot.nb)
  blockHandler(pn$plot.nb, pn$plot.nb.changedHandler) # block changed handler
  dispose(pn$plot.nb) #remove plot
  tag(pn$plot.nb, "tabs")[[index]]<-NULL # remove plot object
  unblockHandler(pn$plot.nb, pn$plot.nb.changedHandler) # unblock changed handler
  pn.selectPlotTab(pn, svalue(pn$plot.nb), loadHandler=loadHandler)
}

# set plot tab specifrically
pn.setPlotTab<-function(pn, plotI, loadHandler=NULL) {
  svalue(pn$plot.nb)<-plotI
  pn.selectPlotTab(pn, plotI, loadHandler=loadHandler)
}

#select plot
pn.selectPlotTab<-function(pn, plotI, loadHandler=NULL) {
  if (plotI<=length(tag(pn$plot.nb, "tabs"))) { # make sure this is not when adding a new plot #FIXME
    pn.activatePlot(pn, plotI)
    if (!is.null(loadHandler))
      do.call(loadHandler, list(obj=pn.getPlotTabParam(pn, plotI)))
  }
}

# activate graphics widget of a plot index
pn.activatePlot<-function(pn, plotI) {
  gg<-pn.getPlotTabParam(pn, plotI, params="gg") # set ggraphics visible
  visible(gg)<-TRUE
}

# reactivate currently selected graphics widget
pn.reactivatePlot<-function(pn) {
  pn.activatePlot(pn, svalue(pn$plot.nb))
}

#get plot properti(es) for a tab
#params as c("test", "test2")
pn.getPlotTabParam<-function(pn, index, params=NULL) {
  if (is.null(params))
    return (tag(pn$plot.nb, "tabs")[[index]])
  else if (length(params)==1)
    return (tag(pn$plot.nb, "tabs")[[index]][[params]])
  else
    return (tag(pn$plot.nb, "tabs")[[index]][params])
}

# get all plot tab objs
pn.getAllPlotTabObjs<-function(pn) return (tag(pn$plot.nb, "tabs"))

# get all plot tab names
pn.getAllPlotTabNames<-function(pn) return(names(pn$plot.nb))

# get selected plot tab name
pn.getSelectedPlotTabName<-function(pn) return(names(pn$plot.nb)[svalue(pn$plot.nb)])

# get them for the selected tab
pn.getSelectedPlotTabParam<-function(pn, params=NULL) return (pn.getPlotTabParam(pn, svalue(pn$plot.nb), params=params))

# set plot properti(es)
# params as list
pn.setPlotTabParam<-function(pn, index, params) {
  for (var in names(params))
    tag(pn$plot.nb, "tabs")[[index]][var]<-params[var]
}

# set them for the selected tab
pn.setSelectedPlotTabParam<-function(pn, params) pn.setPlotTabParam(pn, svalue(pn$plot.nb), params)

# utility function for storing user information within the current tab (with id "plotinfo")
# info = list of parameters
pn.storeInfo<-function(pn, info, reset=FALSE) {
  if (reset)
    plotinfo<-list()
  else
    plotinfo<-pn.getSelectedPlotTabParam(pn, params="plotinfo")
  for (name  in names(info))
    plotinfo[name]<-info[name]
  pn.setSelectedPlotTabParam(pn, list(plotinfo=plotinfo)) # save plot parameter
}

# utility function for retrieving all user information from the current tab
pn.getAllInfo<-function(pn) return(pn.getSelectedPlotTabParam(pn, params="plotinfo"))

# utility function for retrieving parts of the user information from the current tab
pn.getInfo<-function(pn, fields) return(pn.getAllInfo(pn)[fields])

