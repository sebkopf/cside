HubModule = setRefClass(
  'HubModule', contains='Module', 
  fields = list (), 
  methods = list(
    initialize = function(...){
      # define GUI class
      callSuper(gui = HubGUI(), ...)
      
      # settings
      setSettings(
        windowSize = c(400, 400),
        windowTitle = "CSIDE"
      )
    }
  )
)