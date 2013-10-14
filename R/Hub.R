Hub = setRefClass(
  'Hub', contains='Module', 
  fields = list(
    dataManager = 'DataManager', 
    reportBuilder = 'ReportBuilder', 
    interfaceClasses = "list", 
    activeModule = 'Module'
  ), 
  methods = list(
    initialize = function(...) {
      callSuper(...)
      standalone <<- TRUE
    },
    
    #' Setting a directory propagates to all other modules this hub coordinates.
    #' @seealso \code{Module}
    setDir = function(key, dir) {
      callSuper(key, dir)
      
    },
    
    launchDataManager = function () activateModule(dataManager),
    launchReportBuilder = function() activateModule(reportBuilder),
    activateModule = function(module) {
      activeModule$hideGUI()
      activeModule <<- module
      module$showGUI(.self)
    },
    #launchDataManager = function() launchModule(dataManagerClass),
    #launchReportBuilder = function() launchModule(reportBuilderClass),
    #launchModule = function(class) {
    #  mod <- new(class, hub = .self)
    #  mod$speak()
    #  mod
    #},
    getPath = function(to) dirs[[to]]
  )
)

#str(Hub$new()$launchDataManager()$speak())
#str(Hub$new()$launchReportBuilder()$speak())

hub<-Hub$new()
#print(hub$launchDataManager()$getWurst())
#hub$launchReportBuilder()
#hub$launchDataManager()
hub$setHome("/Users/SKOPF/")
print(hub$dirs$home)
print(hub$dataManager$dirs$home)
