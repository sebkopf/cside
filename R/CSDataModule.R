# compound specific data interface
CSDataModule = setRefClass(
  'CSDataModule', contains='DataModule', 
  fields = list(), 
  methods = list(
    initialize = function(...){
      callSuper(...);
    }
  ))