#######################################
# Code for isodat file parser (IDP)   #
# Main file (launch, loading, saving) #
# Copyright 2013 Sebastian Kopf       #
# seb.kopf@gmail.com                  #
#######################################

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this program and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

# launch the program
# parent paramter important if launched from another application
IDP<-function() {
  IDP.libs()
  IDP.source(dir="/Users/SKOPF/Dropbox/Tools/software/R/idp", sourcefile="IDP.R")
  IDP.start(load = "idp", parent=parent)
  return()
}

# launch the program in development mode (repackages)
IDP.dev<-function() {
  IDP.package()
  IDP.libs()
  IDP.source(dir="/Users/SKOPF/Dropbox/Tools/software/R/idp", sourcefile="IDP.R")
  obj <- IDP.start(load = "idpdev", askReload = FALSE)
  return(obj)
}

# launch the program externally (e.g. from CSIDE)
IDP.ext<-function(parent, modal) {
  IDP.libs()
  return(IDP.start(load = NULL, parent=parent, modal=modal))
}

################
# Installation #
################

# install the necessary packages
IDP.install<-function() {
  local({# set mirror to berkely
    r <- getOption("repos")
    r["CRAN"] <- "http://cran.cnr.berkeley.edu/"
    options(repos = r)
  }) 
  cat("Installing required packages.\n")
  install.packages("ggplot2", depen=TRUE) # for advanced plotting, includes reshape2 and plyr, is quick
  install.packages("plyr", depen=TRUE)
  install.packages("psych", depen=TRUE) # for reading the from the clipboard, fairly quick
  install.packages("gWidgets", depen=TRUE) # for user interfaces, quick
  install.packages("xlsx", depen=TRUE) # for excel exports
  update.packages(ask=FALSE)
  cat("All required packages installed.\n")
}

###########################
# Change program settings #
###########################

# change settings on the idp object
IDP.changeSettings<-function(idp, settings) {
  #FIXME - implement me
  return (idp)
}

####################################
# Source, start and initialization #
####################################

IDP.libs <- function() {
  # required libraries
  library(psych)
  library(gWidgets)
  library(ggplot2)
  library(gridExtra)
  library(reshape2)
  library(plyr)
  library(scales)
  library(stringr)
  library(RGtk2)
  library(xlsx)
}

# requried libraries and sourcing source files
IDP.source <- function(dir, sourcefile) {  
  # gui toolkit
  options("guiToolkit"="RGtk2")
  
  # make sure the right id function (from the widgets package) is always used
  id <- gWidgets::id
  
  # sourcing program file
  source(file.path(dir, sourcefile))
}

# start the program
# [idp] = previous instance of the program OR name of the global variable to look for
IDP.start<-function(load = NULL, askReload = TRUE, parent=NULL, modal=FALSE) {
 
  # initialization
  if (identical(class(load), "list"))
    idp <- load # an existing instance was passed along (FIXME: do more checking here?)
  else if (identical(class(load), "character") && 
             exists(load, envir=.GlobalEnv) && 
             (askReload == FALSE || gconfirm("A previous instance of the Isodat Processor is saved in this workspace. Would you like to load it?"))) {
    idp <- get(load, envir=.GlobalEnv) # load from a global variable
  } else
    idp <- IDP.init() # initialize a new instance
    
  # update gvar setting if one is given
  if (identical(class(load), "character"))
    idp$settings$gvar <- load
    
  # update object instance  
  idp <- IDP.update(idp) # update instance
  
  # set parent and modal
  idp$gui$parent<-parent
  idp$gui$modal<-modal
  
  # launch gui
  idp <- IDP.gui(idp)
  
  return (idp)
}

# initialize an idp instance
IDP.init<-function() {
  idp<-list(
    settings = list(),
    gui = list(),
    data = list())
  
  idp$settings$version <- 0.1 # version number
  idp$settings$gvar <- "idp" # global variable name
  idp$settings$options <- list( # which options are available
    saveToWorkspace = TRUE) # button to save IDP to workspace
  idp$settings$mode <- "ModeInfo" # options: ModeInfo, ModeAdd, ModeEdit, ModeDel, ModeStds
  idp$settings$fileDirectory <- getwd() # which directory to start the file browser in
  idp$settings$leftPane <- 0.5 # position of left pane top vs bottom
  idp$settings$rightPane <- 0.6 # position of right pane top vs bottom
  idp$settings$centerPane <- 0.25  # position of center pane left vs right
  
  # plotting options
  idp$settings$plotOptions<-list(
    labels = list(x="Time", y="Signal"), 
    xUnits = list(value = 1, ids = c("XaxisSec", "XaxisMin"), labels = c("s", "min"), funcs = c(function(x) x, function(x) x/60), revFuncs = c(function(x) x, function(x) x*60)),
    yUnits = list(value = 1, ids = c("YaxismV", "YaxisV"), labels = c("mV", "V"), funcs = c(function(x) x, function(x) x/1000)), # y units not currently implemented
    trace2 = list(on = TRUE, color="black", offset=200), #offset in mV
    trace3 = list(on = TRUE, color="dark green", offset=0),
    markRefs = TRUE, # whether standards should be marked
    baseMarker = list(on = TRUE, color="red"),
    apexMarker = list(on = TRUE, color="red"),
    edgeMarker = list(on = TRUE, color="blue"),
    zoomBuffer = 0.05,  # when it will be considered to be a y zoom or just x zoom (currently 5%)
    zoomIn = 0.5, # how much to zoom in (50%)
    zoomOut = 2, # how much to zoom out (100%)
    zoomMove = 0.2 # how much to move interval (20%) 
  )
  
  # FIXME: implement ordering here
  idp$settings$peakTableColumns<-data.frame(
    Name=c("Filename" ,"PeakNr", "RefPeak", "Status", "ID" ,"Component" ,"Master" ,"RefName" ,"Start" ,"Rt" ,"End" ,"Width" ,"Amp2" ,"Amp3" ,"BGD2" ,"BGD3" ,"AreaAll" ,"Area2" ,"Area3" ,"rAreaAll" ,"rArea2" ,"rArea3" ,"R3H2v2H2" ,"rR3H2v2H2" ,"rd3H2v2H2" ,"d3H2v2H2" ,"DeltaDelta3H2v2H2" ,"R2H1H" ,"d2H1H" ,"AT2H1H"),
    Column=c("Filename" ,"Peak Nr.", "Ref. Peak", "Status", "ID" ,"Component" ,"Master Peak" ,"Ref. Name" ,"Start" ,"Rt" ,"End" ,"Width" ,"Ampl. 2" ,"Ampl. 3" ,"BGD 2" ,"BGD 3" ,"Area All" ,"Area 2" ,"Area 3" ,"rArea All" ,"rArea 2" ,"rArea 3" ,"R 3H2/2H2" ,"rR 3H2/2H2" ,"rd 3H2/2H2" ,"d 3H2/2H2" ,"DeltaDelta 3H2/2H2" ,"R 2H/1H" ,"d 2H/1H" ,"AT% 2H/1H"),
    Units=c("" ,"" ,"" , "", "", "","" ,"" ,"[s]" ,"[s]" ,"[s]" ,"[s]" ,"[mV]" ,"[mV]" ,"[mV]" ,"[mV]" ,"[Vs]" ,"[Vs]" ,"[Vs]" ,"[mVs]" ,"[mVs]" ,"[mVs]" ,"" ,"" ,"[per mil] vs. ref" ,"[per mil] vs. VSMOW" ,"" ,"" ,"[per mil] vs. VSMOW" ,"[%]"),
    Type=c("character", "integer", "logical", "character", "integer", "character", "character", "character", rep("numeric", 22)),
    Show=TRUE, Required=FALSE, IsodatCol=TRUE, stringsAsFactors=FALSE)
  idp$settings$peakTableColumns[2, "Required"]<-TRUE # Peak Nr. is required b/c it's the ID
  idp$settings$peakTableColumns[3:5, "IsodatCol"]<-FALSE # Columns not present in isodat
  idp$settings$peakTableColumns[3:5, "Show"]<-FALSE # do not show these by default
  return (idp)
}

############
# Settings #
############

# get settings (all or selected subset)
# settings = c("setting1", "setting2")
IDP.getSettings<-function(idp, settings=NULL) {
  if (is.null(settings))
    return (tag(idp$gui$win, "settings"))
  else if (length(settings)==1)
    return (tag(idp$gui$win, "settings")[[settings]])
  else
    return (tag(idp$gui$win, "settings")[settings])
}

# set settings
# settings - list of settings to change
# --> list(gvar="1.2", fileDirectory="/Users/")
# HINT: understands . as a level separator
# --> e.g., list(plotOptions.zoomIn = 0.3, plotOptions.baseMarker.on = FALSE) 
IDP.setSettings<-function(idp, settings) {
  for (var in names(settings)) {
    levels <- unlist(strsplit(var, "\\."))
    evalStr <- paste(paste("[[levels[", 1:length(levels), "]]]", sep=""), collapse="")
    evalstr <- paste("tag(idp$gui$win, 'settings')", evalStr, "<-settings[[var]]", sep="")
    eval(parse(text=evalstr))
  }
}


#######################
# Saving to workspace #
#######################

# saves idp instance to the gvar stored in it
IDP.save <- function(idp) {
  save <- IDP.init()
  IDP.setSettings(idp, list(
    leftPane = svalue(idp$gui$gl), 
    rightPane = svalue(idp$gui$gr), 
    centerPane = svalue(idp$gui$gall)))
  save$settings <- IDP.getSettings(idp) # get newest settings back from window instance
  
  # FIXME: also save data
  # Note: not saving any of the gui components (no need to keep the pointers)
  assign(idp$settings$gvar, save, envir=.GlobalEnv)
}

##########################
# Versioning and Updates #
##########################

# get all versions of the software
IDP.getVersions<-function() {
  # versions data frame
  versions<-data.frame(version=numeric(), date=character(), released=logical(), codename=character(), stringsAsFactors=F)
  versions[nrow(versions)+1,]<-list(version=0.1, date="8/22/2013", released=FALSE, codename="Rumpelstilzchen")
  #versions[nrow(versions)+1,]<-list(version=0.2, date="8/22/2013", released=TRUE, codename="Wurst")
  #versions[nrow(versions)+1,]<-list(version=0.3, date="8/22/2013", released=FALSE, codename="Test")
  # label
  versions$label<-paste("version ", versions$version, " (", versions$date, ", codename ", versions$codename, ")", sep="")
  versions[which(!versions$released),]$label <- paste(versions[which(!versions$released),]$label, "- DEV VERSION (unreleased)")
  return (versions)
}

# get version
IDP.getVersionInfo<-function(idp) {
  return (subset(IDP.getVersions(), version==idp$settings$version, select="label"))
}

# update an idp instance to the newest version
IDP.update<-function(idp) {
  for (version in subset(IDP.getVersions(), version > idp$settings$version)$version) {
    cat("Updating IDP instance from version", idp$settings$version, "to version", version, "...")
    idp<-do.call(paste("IDP.updateTo.v", version, sep=""), args=list(idp))
    cat(" complete.\n")
    idp$settings$version <- version
  }
  return (idp)
}

# update functions
IDP.updateTo.v0.2<-function(idp) {
  return (idp)
}

###################
# Code management #
###################

# package IDP
IDP.package<-function(filename="IDP.R", dir="/Users/SKOPF/Dropbox/Tools/software/R/idp") {
  funcsdir<-"/Users/SKOPF/Dropbox/Tools/software/R/funcs"
  funcsfiles<-c("SKGUILIB.R", "SKDATALIB.R", "SKUTILLIB.R", "SKPLOTLIB.R")
  idpdir<-dir
  idpfiles<-c("IDP.main.R", "IDP.gui.R", "IDP.plots.R", "IDP.read.R", "IDP.table.R", "IDP.calcs.R", "IDP.export.R")
  files<-c(file.path(idpdir, idpfiles), file.path(funcsdir, funcsfiles))
  
  header<-paste(
    "##################################\n",
    "# PACKAGED Isodat File Processor #\n",
    "# ", format(Sys.time(), "%Y-%m-%d"), " by SKOPF           #\n",
    "##################################\n\n", sep="")
  for (i in 1:length(files)) 
    files[i]<-readChar(files[i], file.info(files[i])$size)
  cat(header, files, file=file.path(dir, filename), sep="\n\n\n")
}

