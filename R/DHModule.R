# implements D/H isodat data
DHModule = setRefClass(
  'DHModule', contains='CSDataModule', 
  fields = list(), 
  methods = list(
    initialize = function(...){
      callSuper(...)
      
      # plotting options
      plotOptions = list(
        labels = list(x="Time", y="Signal"), 
        xUnits = list(value = 1, ids = c("XaxisSec", "XaxisMin"), labels = c("s", "min"), funcs = c(function(x) x, function(x) x/60), revFuncs = c(function(x) x, function(x) x*60)),
        yUnits = list(value = 1, ids = c("YaxismV", "YaxisV"), labels = c("mV", "V"), funcs = c(function(x) x, function(x) x/1000)), # y units not currently implemented
        trace2 = list(on = TRUE, color="black", offset=200), #offset in mV
        trace3 = list(on = TRUE, color="dark green", offset=0),
        markRefs = TRUE, # whether standards should be marked
        baseMarker = list(on = TRUE, color="red"),
        apexMarker = list(on = TRUE, color="red"),
        edgeMarker = list(on = TRUE, color="blue"),
        zoomBuffer = 0.05,  # when it will be considered to be a y zoom or just x zoom (currently 5%)
        zoomIn = 0.5, # how much to zoom in (50%)
        zoomOut = 2, # how much to zoom out (100%)
        zoomMove = 0.2 # how much to move interval (20%) 
      )
      
      # peak table settings
      peakTableColumns <- data.frame(
        Name=c("Filename" ,"PeakNr", "RefPeak", "Status", "ID" ,"Component" ,"Master" ,"RefName" ,"Start" ,"Rt" ,"End" ,"Width" ,"Amp2" ,"Amp3" ,"BGD2" ,"BGD3" ,"AreaAll" ,"Area2" ,"Area3" ,"rAreaAll" ,"rArea2" ,"rArea3" ,"R3H2v2H2" ,"rR3H2v2H2" ,"rd3H2v2H2" ,"d3H2v2H2" ,"DeltaDelta3H2v2H2" ,"R2H1H" ,"d2H1H" ,"AT2H1H"),
        Column=c("Filename" ,"Peak Nr.", "Ref. Peak", "Status", "ID" ,"Component" ,"Master Peak" ,"Ref. Name" ,"Start" ,"Rt" ,"End" ,"Width" ,"Ampl. 2" ,"Ampl. 3" ,"BGD 2" ,"BGD 3" ,"Area All" ,"Area 2" ,"Area 3" ,"rArea All" ,"rArea 2" ,"rArea 3" ,"R 3H2/2H2" ,"rR 3H2/2H2" ,"rd 3H2/2H2" ,"d 3H2/2H2" ,"DeltaDelta 3H2/2H2" ,"R 2H/1H" ,"d 2H/1H" ,"AT% 2H/1H"),
        Units=c("" ,"" ,"" , "", "", "","" ,"" ,"[s]" ,"[s]" ,"[s]" ,"[s]" ,"[mV]" ,"[mV]" ,"[mV]" ,"[mV]" ,"[Vs]" ,"[Vs]" ,"[Vs]" ,"[mVs]" ,"[mVs]" ,"[mVs]" ,"" ,"" ,"[per mil] vs. ref" ,"[per mil] vs. VSMOW" ,"" ,"" ,"[per mil] vs. VSMOW" ,"[%]"),
        Type=c("character", "integer", "logical", "character", "integer", "character", "character", "character", rep("numeric", 22)),
        Show=TRUE, Required=FALSE, IsodatCol=TRUE, stringsAsFactors=FALSE)
      peakTableColumns[2, "Required"]<-TRUE # Peak Nr. is required b/c it's the ID
      peakTableColumns[3:5, "IsodatCol"]<-FALSE # Columns not present in isodat
      peakTableColumns[3:5, "Show"]<-FALSE # do not show these by default
      
      # settings
      setSettings(
        windowSize = c(800, 200),
        windowTitle = "IDP - Isodata Data Processor",
        launchIcon = "gtk-page-setup",
        launchName = "D/H Delta+",
        
        version = 0.1, # version number # FIXME keep track of this differently!
        mode = "ModeInfo", # options: ModeInfo, ModeAdd, ModeEdit, ModeDel, ModeStds
        leftPane = 0.5, # position of left pane top vs bottom
        rightPane = 0.6, # position of right pane top vs bottom
        centerPane = 0.25,  # position of center pane left vs right
        
        plotOptions = plotOptions, 
        peakTableColumns = peakTableColumns
      )
    }
  ))