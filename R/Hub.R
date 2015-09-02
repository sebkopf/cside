#' @include DataElement.R
NULL

Hub = setRefClass(
  'Hub', 
  contains='DataElement', 
  fields = list(
    modules = 'list', # list of Module objects
    activeModule = 'character'
  ), 
  #lock = list(modules), #FIXME implement this
  methods = list(
    initialize = function(modules, ...) {
      
      # start hub
      callSuper(modules = modules, activeModule = 'none', ...)
      
      # propagate module names to the GUIs
      lapply(names(modules), function(name) {
        module <- modules[[name]]
        module$gui@module <- name
      })
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
        hideGui(getModule()$gui, .self) # hide the active module
      
      # show new module
      showGui(getModule(name)$gui, .self) # show new module
      activeModule <<- name
    },
    
    # launches the first module by default
    launchHub = function() { 
      if (length(modules) == 0)
        stop("this hub has no modules registered")
      launchModule(names(modules)[1]) 
    },
    
    #' Send data to a module
    sendDataToModule = function(id) {
      print("not implemented yet")
      print(id)
    }
  )
)
