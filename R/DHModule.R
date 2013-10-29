# implements D/H isodat data
DHModule = setRefClass(
  'DHModule', contains='CSDataModule', 
  fields = list(), 
  methods = list(
    initialize = function(...){
      callSuper(...)
      
      # settings
      setSettings(
        windowSize = c(800, 200),
        windowTitle = "IDP - Isodata Data Processor",
        launchIcon = "gtk-page-setup",
        launchName = "D/H Delta+"
      )
    }
  ))