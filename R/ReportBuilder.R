ReportBuilder = setRefClass(
  'ReportBuilder', contains='Module', 
  fields = list (), 
  methods = list(
    initialize = function(...){
      callSuper(...) #FIXME: define GUI
    }
  )
)