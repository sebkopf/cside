# compound specific data interface
CSDataInterface = setRefClass(
  'CSDataInterface', contains='DataInterface', 
  fields = list(), 
  methods = list(
    initialize = function(){
      callSuper();
    }
  ))