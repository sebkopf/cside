#' Class representing a CSIDE Data Frame that holds two type of information data and settings.
#' A container for basic data information. 
#' 
DataFrame <- setRefClass(
  'DataFrame', 
  fields = list(
    data = 'list',
    settings = 'list'
  ), 
  methods = list(
    
    #' Get settings
    getSettings = function(id) {
      message("get Settings not implemented yet")
    },
    
    #' Adjust settings - FIXME
    #addParams = function(...){
    #   params <<- modifyList(params, list(...))
    #},
    
    getSetting = function(id) {
      return (settings[[id]])
    },
    
    getSettings = function(ids) {
      if (missing(ids))
        return (settings)
      return (settings[ids])
    },
    
    setSettings = function(...) {
      # FIXME: allow both ... and list(a=b) to be passed in!
      settings <<- modifyList(settings, list(...))
    },
    
    #' Get Data
    getData = function(id) {
      message("get Data not implemented yet")
    }
  )
)
