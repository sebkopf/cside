DataManager = setRefClass(
  'DataManager', contains='Module', 
  fields = list (), 
  methods = list(
    initialize = function(...){
      callSuper(gui = DataManagerGUI(), ...)
      
      # settings
      settings$windowSize <<- c(800, 200)
      settings$windowTitle <<- "CSIDE - Data Manager"
    }
  )
)