ReportBuilder = setRefClass(
  'ReportBuilder', contains='Module', 
  fields = list(hans = 'character'), 
  methods = list(
    initialize = function(...){
      callSuper(...)
      hans <<- "hello"
    },
    getHans = function(...){
      hans
    }
  ))