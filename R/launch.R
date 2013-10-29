cside <- function(
  home = "/Users/SKOPF/Dropbox/Tools/software/R",
  dataManager = DataManager$new(),
  reportBuilder = ReportBuilder$new(),
  dataModules = list(
    gcms = GcMsFidModule$new(),
    dh = DHModule$new())) {
  
  cat("\nInitializing CSIDE ...\n")
  
  # initialize CSIDE hub
  hub <- Hub$new(dataManager = dataManager, reportBuilder = reportBuilder, dataModules = dataModules)
  
  # set starting home directory
  hub$setHome(home)
  hub$launchHub()
  #hub$launchDataManager()
  #hub$dataManager$checkActive()
  
  message("CSIDE done")
  return(hub)
}

#cside()

# FIXME: modal implementation!!

### FIXME - implement me:
# This is how I should implement the modal part!

# library(RGtk2)
# 
# # this is a brilliant way of launching the user interfaces
# button <- gtkButton ( "Click Me" )
# button['image'] <- gtkImage(stock = "gtk-apply", size = "button") 
# gSignalConnect(button,"clicked" , function ( button ) {
#   message ( "Hello World!" ) 
#   gtkMainQuit()
# })
# ##
# window <- gtkWindow(show = FALSE)
# window$add ( button )
# window$showAll()
# gtkMain()
# # I think this is the key to running in modal only at specific times


# FOR COMMAND LINE, incorporate this:
####################################
# CSIDE                            # 
# application launcher             #
# Written by Sebastian Kopf, 2013  #
# seb.kopf@gmail.com               #
####################################

# How to use this launcher.

#####
# For installing CSIDE:
# It is highly recommended to run this launcher via Rscript (a) for the installation (to avoid package trouble with your currently running session).
# However, you also have the option to do it in an R session (b).
###
# a) via Rscript (no knowledge of R required)
# 1.) Open terminal (mac/linux) or cmd (windows)
# 2.) Run the following:
# > {Rscript} --vanilla {CSIDE} --install 
# where {Rscript} is the path to the R executable and {CSIDE} the path to the CSIDE launcher
# example on Mac (if Rscript is in your path and CSIDE launch in /Users/BOB/Documents/CSIDE/CSIDE.launch.R): 
#     > Rscript --vanilla /Users/BOB/Documents/CSIDE/CSIDE.launch.R --install
# example on Windows (if Rscript is in C:\Program Files\R\R-3.0.1\bin\R.exe and CSIDE launch on your desktop in C:\Documents and Settings\BOB\Desktop\CSIDE\CSIDE.launch.R)
#     > C:\Program Files\R\R-3.0.1\bin\R.exe --vanilla C:\Documents and Settings\BOB\Desktop\CSIDE\CSIDE.launch.R --install
# 3.) Done (you can check the log file in the CSIDE directory if you are curious or there is any trouble)
###
# b) in R session or Rstudio (assumes some knowledge of R)
# 1.) Source this file.
# 2.) Run CSIDE.install()
# 3.) Done.
#####

#####
# For running CSIDE:
# Again you have the two options - Rscript (a) or as R session/Rstudio (b)
###
# a) via Rscript (no knowledge of R required)
# 1.) Open terminal (mac/linux) or cmd (windows)
# 2.) Run the following:
# > {Rscript} --vanilla {CSIDE} -startWD "{startwd}" -sourceDir "{sourcedir}" --repackage
# where {Rscript} is the path to the R executable and {CSIDE} the path to the CSIDE launcher
# -startWD is an optional parameter for the starting workspace diretory (if not specified, the CSIDE folder will be used)
# -sourceDir is an optional paramter for the CSIDE source code folder (if not specified, the CSIDE folder will be used)
# --repackage is an optional paramter for whether to repackage the sources (essentially for development mode)
# example on Mac (if Rscript is in your path and CSIDE launch in /Users/BOB/Documents/CSIDE/CSIDE.launch.R): 
#     > Rscript --vanilla /Users/BOB/Documents/CSIDE/CSIDE.launch.R 
# example on Windows (if Rscript is in C:\Program Files\R\R-3.0.1\bin\R.exe and CSIDE launch on your desktop in C:\Documents and Settings\BOB\Desktop\CSIDE\CSIDE.launch.R)
#     > C:\Program Files\R\R-3.0.1\bin\R.exe --vanilla C:\Documents and Settings\BOB\Desktop\CSIDE\CSIDE.launch.R 
# 3.) Done (you can check the log file in the CSIDE directory if you are curious or there is any trouble)
###
# b) in R session or Rstudio (assumes some knowledge of R)
# 1.) Change the following variables to the appropriate directory
startWD <- "/Users/SKOPF/Dropbox/Tools/software/R" # in which directory the program should start by default ("" =  whichever working directory you are in when you start CSIDE)
sourceDir <- "/Users/SKOPF/Dropbox/Tools/software/R/CSIDE" # in which directory the source code for the CSIDE application is located ("" = whichever working directory you are in when you start CSIDE)
#  b) Source this file (CSIDE.launch.R) in your R session by running this code (replace to have the approriate directory):
#     source("...../CSIDE.launch.R")
#  c) run the CSIDE function (will take startDir and sourceDir you define above by default but can take many other paramters - check out the CSIDE function below for details)
#     CSIDE()

##############
### CSIDE ###
# Program CODE
# startWD - working directory to start CSIDE in (default startWD defined above)
# sourceDir - path to source code directory of CSIDE (default sourceDir defined above)
# version - version of CSIDE to launch (default = newest version)
# repackage - whether to repackage all the sources (in case something has changed)
# modal [FALSE] - whether CSIDE is launched modally (no other interaction with that R session possible)
noCSIDE <- function(startWD = NULL, sourceDir = NULL, version = NULL, repackage = FALSE, modal=FALSE) {
  if (missing(startWD))
    startWD <- get("startWD", envir=.GlobalEnv)
  if (missing(sourceDir))
    sourceDir <- get("sourceDir", envir=.GlobalEnv)
  if (missing(version)) { 
    codefiles <- dir(path=sourceDir, pattern="^CSIDE-\\d+\\.\\d+\\.R$", include.dirs=FALSE)
    version <- max(as.numeric(gsub("^CSIDE-(\\d+\\.\\d+)\\.R$", "\\1", codefiles)))
  }
  CSIDEsource <- file.path(sourceDir, paste("CSIDE-", version, ".R", sep=""))
  
  # setup working directory
  cat("\nLoading starting working directory ...\n")
  setwd(startWD)
  cat(startWD, "loaded.\n")
  
  # sourcing CSIDE
  cat("\nSourcing CSIDE version ", version, " ...\n", sep="")
  CSIDE.source(CSIDEsource)
  
  # repackage
  if (repackage) {
    CSIDE.package(dir=sourceDir)
    CSIDE.source(CSIDEsource) # also reload
  }
  
  # initiazlizing GUI
  cat("\nInitializing CSIDE ...\n")
  CSIDE.init(CSIDEsource, modal)
}

CSIDE.source <- function(src) {
  source(src)
  cat(src, "loaded.\n")
}

# development convenience function (always repackages)
CSIDE.dev<-function() CSIDE(repackage=TRUE, modal=FALSE)

# installation function
# cran - which cran mirror to use, default is berkeley
# packages - this should contain all of them
#   ggplot2 - for advanced plotting, includes reshape2 and plyr, is quick
#   plyr - for data restructuring 
#   zoo - for rolling means calculations
#   psych - for reading the from the clipboard
#   sqldf - for running sql statements on data frames
#   gWidgets - for user interface
#   gridExtra - for grid layout of tables
#   xlsx - for Excel interaction
CSIDE.install<-function(
  cran = "http://cran.cnr.berkeley.edu/",
  packages = c("ggplot2", "plyr", "zoo", "psych", "sqldf", "gWidgets", "gridExtra", "xlsx")) {
  
  cat("\nSetting CRAN repository to ", cran)
  local({# set mirror to berkely
    r <- getOption("repos")
    r["CRAN"] <- cran
    options(repos = r)
  }) 
  cat("\nInstalling required packages ...\n")
  
  installPackage <- function(name) {
    cat("\nInstalling package ", name, ":\n")
    install.packages(name, depen=TRUE)
  }
  
  for (package in packages)
    installPackage(package)
  update.packages(ask=FALSE)
  cat("\nAll required packages installed.\n\n")
}

#######################################################
# CSIDE launcher function for command line exection   #
# onl relevant for command line execution via Rscript #
#######################################################
CSIDE.launch <- function () {
  # check whether this file is being invoked as an rscript launcher
  
  cmd_args = commandArgs()
  if (length(fileArg <- grep("^--file=.*CSIDE\\.launch\\.R$", cmd_args, value=TRUE)) > 0 ) {
    # get the absolute path to the directory the launcher was executed from
    launcherDir <- 
      do.call("file.path", arg=as.list(
        head(strsplit(substring(fileArg, 8), .Platform$file.sep)[[1]], n=-1)))
    
    # we are in command line mode, go send the output to a log file as well
    if (is.na(file.info(file.path(launcherDir,"logs"))$size)) dir.create(file.path(launcherDir, "logs")) # logs folder
    logFile <- file.path(launcherDir, "logs", paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), ".CSIDE.log")) # log file
    sink(logFile, split = TRUE)
    cat("\nStarting log file ", logFile, "\n\n")
    cat("\nWelcome to CSIDE\n")
    
    # check for installing
    if (length(grep("^--install$", cmd_args)) > 0 ) {
      cat("\nInstalling CSIDE ...")
      CSIDE.install()
      cat("CSIDE installation completed succesfully.\nHave fun.\n\n")
    } else {
      # starting program instead
      cat("\nStarting CSIDE ...")
      
      # get start dir
      if ( length(startWDI <- grep("^-startWD$", cmd_args)) > 0)
        startWD <- cmd_args[startWDI + 1]
      else
        startWD <- launcherDir
      
      # get source dir
      if ( length (sourceDirI <- grep("^-sourceDir$", cmd_args)) > 0)
        sourceDir <- cmd_args[sourceDirI + 1]
      else 
        sourceDir <- launcherDir
      
      # repackage
      repackage <- (length(grep("^--repackage$", cmd_args)) > 0)
      
      # start
      CSIDE(startWD = startWD, sourceDir = sourceDir, repackage=repackage, modal=TRUE)
      cat("\nBye bye.\n")
    }
  } else
    cat("\nCSIDE launcher sourced succesfully.\nStart CSIDE by calling cside().\nHave fun!\n\n")
}
#CSIDE.launch() # launch CSIDE (only does something if this is called via Rscript)


