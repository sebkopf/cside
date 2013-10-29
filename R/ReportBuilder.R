ReportBuilder = setRefClass(
  'ReportBuilder', contains='Module', 
  fields = list (), 
  methods = list(
    initialize = function(...){
      callSuper(...) #FIXME: define GUI
      
      # settings
      setSettings(
        windowTitle = "CSIDE - Report Builder",
        launchIcon = "gtk-find-and-replace",
        launchName = "Reports"
      )
    }
  )
)