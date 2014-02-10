#' A hub is a DataElement that has multiple modules associated with it that it can launch.
#' It's the main object passed to all the GUIs but in the GUIs then always returns the appriate module that belongs to the guy
#' while retaining the ability to launch other modules from anywhere in the GUI.
#' 
Hub = setRefClass(
  'Hub', 
  contains='DataElement', 
  fields = list(
    modules = 'list', # list of Module objects
    activeModule = 'character'
  ), 
  #lock = list(modules), #FIXME implement this
  methods = list(
    initialize = function(...) {
      # start hub
      callSuper(activeModule = 'none', ...)
      
      # propagate module names to the GUIs
      lapply(names(modules), function(name) {
        module <- modules[[name]]
        module$gui@module <- name
      })
      
      # safety checking
      # FIXME implement checking for modules to be the right data type!
    },
    
    #' Get a module (active one by default)
    getModule = function(name = activeModule) {
      if (identical(name, "none"))
        stop(paste("Trying to get module when none is active (yet)."))
      if (!name %in%names(modules))
        stop(paste("The module", name, "does not exist!"))
      return(modules[[name]])
    },
 
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
