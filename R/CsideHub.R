#' The hub of the CSIDE program
#' 
CsideHub = setRefClass(
  'CsideHub', 
  contains='Hub', 
  methods = list(
    initialize = function(
      startModule = CsideStartModule$new(), 
      dataManager = DataManager$new(), reportBuilder = ReportBuilder$new(), dataModules = list(), 
      home = getwd(), ...) {
      
      # CSIDE hub modules
      callSuper(modules = c(
        list(
          start = startModule,
          dataManager = dataManager,
          reportBuilder = reportBuilder), 
        dataModules), ...)
      
      # hub level settings
      setSettings(
        dirs = list (
          home = home, # home is the launch directory
          wd = home, # the active working directory (=home at the beginning)
          projects = "projects", # data directories
          libs = "libraries", # compound libraries
          settings = "settings", # settings
          reports = "reports" # reports directories
        ))
    },
    
    #' Getting and setting home directory and other directories
    getHome = function() getDir('home'),
    getWD = function() getDir('wd'),
    getDir = function(key) getSetting('dirs')[[key]],
    setWD = function(dir) setSettings(dirs = list('wd' = dir)),
  
    #' Launch all the different standard modules of the CSIDE Hub
    launchStart = function() { launchModule('start') },
    launchDataManager = function () launchModule('dataManager'),
    launchReportBuilder = function() launchModule('reportBuilder'),
    launchDataModule = function(name) launchModule(name)
  )
)
