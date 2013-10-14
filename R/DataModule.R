DataModule = setRefClass(
  'DataModule', contains='Module', 
  fields = list(), 
  methods = list(
    initialize = function(){
      callSuper();
    }
  ))