Hub = setRefClass(
  'Hub', contains='Module', 
  fields = list(
    dataManager = 'DataManager', 
    reportBuilder = 'ReportBuilder', 
    dataModules= 'list', 
    activeModule = 'Module'
  ), 
  methods = list(
    initialize = function(dataManager = DataManager$new(), reportBuilder = ReportBuilder$new(), dataModules = list()) {
      callSuper(standalone = TRUE)
      
      # objects
      dataManager <<- dataManager
      reportBuilder <<- reportBuilder
      dataModules <<- dataModules
      
      # settings
      settings$windowSize <<- c(400, 400)
      settings$windowTitle <<- "CSIDE"
      
      # gui
      guiFunc <<- guiHub
    },
    
    #' Setting a directory propagates to all other modules this hub coordinates.
    #' @seealso \code{Module}
    setDir = function(key, dir) {
      # update directory as well for all modules
      lapply(c(dataManager, reportBuilder, dataModules), FUN=function(module) module$setDir(key, dir))
      callSuper(key, dir)
    },
    
    #' Launch all the different modules
    launchHub = function() {activeModule$hideGUI(); showGUI(.self)},
    launchDataManager = function () launchModule(dataManager),
    launchReportBuilder = function() launchModule(reportBuilder),
    launchDataModule = function(id) launchModule(dataModules[[id]]),
    
    #' Generic module launch function
    launchModule = function(module) {
      hideGUI() # hide hub GUI
      activeModule$hideGUI() # hide any other visible modules
      activeModule <<- module # assign new module
      module$showGUI(.self) # show new module
    }
  )
)
