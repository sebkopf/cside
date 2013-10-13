# implements dual data trace of GC and MS data
GcMsFidInterface = setRefClass(
  'GcMsFidInterface', contains='CSDataInterface', 
  fields = list(), 
  methods = list(
    initialize = function(){
      callSuper();
    }
  ))