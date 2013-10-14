DataManager = setRefClass(
  'DataManager', contains='Module', 
  fields = list (), 
  methods = list(
    initialize = function(...){
      callSuper(...)
    },
    
    #' Show Data Manager GUI
    showGUI = function(hub) {
      callSuper(hub)
      cat("\n Data manger GUI")
    }
  ))