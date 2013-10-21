# implements dual data trace of GC and MS data
GcMsFidModule = setRefClass(
  'GcMsFidModule', contains='CSDataModule', 
  fields = list(), 
  methods = list(
    initialize = function(...){
      callSuper(...);
    }
  ))