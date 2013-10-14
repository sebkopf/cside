DataManager = setRefClass(
  'DataManager', contains='Module', 
  fields = list (), 
  methods = list(
    initialize = function(...){
      callSuper(...)
      
      # settings
      settings$windowSize <<- c(800, 200)
      settings$windowTitle <<- "CSIDE - Data Manager"
      
      # gui
      guiFunc <<- guiDataManager
    }
  )
)