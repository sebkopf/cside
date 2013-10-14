ReportBuilder = setRefClass(
  'ReportBuilder', contains='Module', 
  fields = list(), 
  methods = list(
    initialize = function(...){
      callSuper(...)
    },
    
    #' Show ReportBuilder GUI
    showGUI = function(hub) {
      callSuper(hub)
      cat("\n Report Builder GUI")
    }
  ))