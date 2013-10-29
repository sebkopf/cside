DataManager = setRefClass(
  'DataManager', contains='Module', 
  fields = list (), 
  methods = list(
    initialize = function(...){
      callSuper(gui = DataManagerGUI(), ...)
      
      # settings
      setSettings(
        windowSize = c(800, 200),
        windowTitle = "CSIDE - Data Manager (DSQDP)",
        launchIcon = "gtk-select-color",
        launchName = "DSQDP",
        launchTooltip = "Start the data manager"
      )
    }
  )
)