# implements D/H isodat data
DHModule = setRefClass(
  'DHModule', contains='CSDataModule', 
  fields = list(), 
  methods = list(
    initialize = function(){
      callSuper();
    }
  ))