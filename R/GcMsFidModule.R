# implements dual data trace of GC and MS data
GcMsFidModule = setRefClass(
  'GcMsFidModule', contains='CSDataModule', 
  fields = list(), 
  methods = list(
    initialize = function(...){
      callSuper(...)
      
      # settings
      setSettings(
        windowTitle = "GC-MS FID Module",
        launchIcon = "gtk-convert",
        launchName = "GC-MS/FID"
      )
    }
  ))