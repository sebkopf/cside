DataInterface = setRefClass(
  'DataInterface', contains='Module', 
  fields = list(), 
  methods = list(
    initialize = function(){
      callSuper();
    }
  ))