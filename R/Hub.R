Hub = setRefClass(
  'Hub', 
  contains='DataFrame', 
  fields = list(
    modules = 'list', # list of Module objects
    activeModule = 'character'
  ), 
  #lock = list(modules), #FIXME implement this
  methods = list(
    initialize = function(
        hubModule = HubModule$new(), 
        dataManager = DataManager$new(), reportBuilder = ReportBuilder$new(), dataModules = list(), 
        home = getwd(), ...) {
      
      # start hub
      callSuper(activeModule = 'none', ...)
      
      # modules
      modules <<- c(
        list(
          hub = hubModule,
          dataManager = dataManager,
          reportBuilder = reportBuilder), 
        dataModules)
      
      # propagate module names to the GUIs
      lapply(names(modules), function(name) {
        module <- modules[[name]]
        module$gui@module <- name
      })
      
      # safety checking
      # FIXME implement checking for modules to be the right data type!
      
      # hub level settings
      setSettings(
        dirs = list (
          home = home, # home is the working directory by default
          projects = "projects", # data directories
          libs = "libraries", # compound libraries
          settings = "settings", # settings
          reports = "reports" # reports directories
        ))
    },
    
    #' Setting home directory and other directories
    setHome = function(dir) setDir('home', dir),
    setDir = function(key, dir) setSettings(dirs = list(key = dir)),
    
    #' Get a module (active one by default)
    getModule = function(name = activeModule) {
      if (identical(name, "none"))
        stop(paste("Trying to get module when none is active (yet)."))
      if (!name %in%names(modules))
        stop(paste("The module", name, "does not exist!"))
      return(modules[[name]])
    },
    
    # FIXME: maybe implement?
    #' Get module settings
    #' Get module data
    #' Get module widgets
    
    #' Launch all the different modules
    launchHub = function() { launchModule('hub') },
    launchDataManager = function () launchModule('dataManager'),
    launchReportBuilder = function() launchModule('reportBuilder'),
    launchDataModule = function(name) launchModule(name),
    
    #' Generic module launch function
    launchModule = function(name) {
      # hide active module
      if (!identical(activeModule, 'none'))
        hideGUI(getModule()$gui, .self) # hide the active module
      
      # show new module
      showGUI(getModule(name)$gui, .self) # show new module
      activeModule <<- name
    },
    
    #' Send data to a module
    sendDataToModule = function(id) {
      print("not implemented yet")
      print(id)
    }
  )
)
