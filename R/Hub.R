Hub = setRefClass(
  'Hub', 
  fields = list(
    dataManagerClass = "character", reportBuilderClass = "character", interfaceClasses = "list", dirs = "list"
  ), 
  methods = list(
    initialize = function(...) {
      dataManagerClass <<- "DataManager"
      reportBuilderClass <<- "ReportBuilder"
      interfaceClasses <<- list()
      dirs <<- list (
        projects = "projects", # data directories
        libs = "libraries", # compound libraries
        settings = "settings", # settings
        reports = "reports" # reports directories
      )
      callSuper(...)
    },
    launchDataManager = function() launchModule(dataManagerClass),
    launchReportBuilder = function() launchModule(reportBuilderClass),
    launchModule = function(class) {
      mod <- new(class, hub = .self)
      mod$speak()
      mod
    },
    getPath = function(to) dirs[[to]]
  )
)

#str(Hub$new()$launchDataManager()$speak())
#str(Hub$new()$launchReportBuilder()$speak())

Hub$new()$launchDataManager()$hub$launchReportBuilder()
