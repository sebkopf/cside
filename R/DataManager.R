DataManager = setRefClass(
  'DataManager', contains='Module', 
  fields = list (wurst = 'character'), 
  methods = list(
    initialize = function(...){
      callSuper(...)
      wurst <<- "hello"
    },
    getWurst = function(...){
      wurst
    }
  ))