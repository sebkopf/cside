#####################################
# Graphing functions        				#
# Copyright 2013 by Sebastian Kopf  #
# These are not all mine, sources   #
# indicated where taken from else-  #
# where                             #
#####################################

##############################
# overwrite funtions         #
# fixme: removed as soon as  #
# fixed in package           #
##############################
gglocator<-function (n = 1, object = last_plot(), message = FALSE, xexpand = c(0.05, 0), yexpand = c(0.05, 0)) {
  if (n > 1) {
    df <- NULL
    for (k in 1:n) {
      df <- rbind(df, gglocator(object = object, message = message, 
                                xexpand = xexpand, yexpand = yexpand))
    }
    return(df)
  }
  x <- grid.ls(print = message)[[1]]
  #x <- x[grep("panel-", grid.ls(print = message)[[1]])] #SK CHANGE
  x <- grid.ls()[[1]][grep("panel", grid.ls(print = message)[[1]])][1] #SK CHANGE
  seekViewport(x)
  loc <- as.numeric(grid.locator("npc"))
  xrng <- with(object, range(data[, deparse(mapping$x)]))
  yrng <- with(object, range(data[, deparse(mapping$y)]))
  xrng <- scales::expand_range(range = xrng, mul = xexpand[1], 
                               add = xexpand[2])
  yrng <- scales::expand_range(range = yrng, mul = yexpand[1], 
                               add = yexpand[2])
  point <- data.frame(xrng[1] + loc[1] * diff(xrng), yrng[1] + 
                        loc[2] * diff(yrng))
  names(point) <- with(object, c(deparse(mapping$x), deparse(mapping$y)))
  point
}


####### useful constants #########
#color-blind friendly palettes:
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# Preferred shapes for filling (they can take a fill colour)
prefShapes<-21:25
# e.g.: #ggplot(data.frame(x=1:5, y=10:14, colour=1:5), aes(x=x, y=y, fill=factor(colour), shape=factor(colour))) + geom_point(colour="black", size=5) + scale_shape_manual(values=21:25)

# simple histogram whisker plot combination function 
# data=supplied as a simple vector
plot.whiskerhist<-function(data, xlab, filename="whiskerhist.pdf") {
	maxF=max(hist(data)$counts)
	pdf(filename)
	h<-hist(data,main='',xlab=xlab,ylim=c(0,1.5*maxF))
	boxplot(data,horizontal=TRUE,at=1.125*maxF,add=TRUE,axes=FALSE)
	stripchart(data,add=TRUE,at=1.375*maxF)
	axis(3)
	dev.off()
}

# error bars
plot.addErrorBars<-function(q, err, y, color="black", width=1, linetype=1) {
	if (is.na(color) && is.na(linetype))
		q<-q + geom_errorbar(aes_string(ymax = paste(y,"+",err), ymin=paste(y,"-",err)),width=width)
	else if (is.na(linetype))
		q<-q + geom_errorbar(aes_string(ymax = paste(y,"+",err), ymin=paste(y,"-",err)),width=width,color=color)
	else if (is.na(color))
		q<-q + geom_errorbar(aes_string(ymax = paste(y,"+",err), ymin=paste(y,"-",err)),width=width,linetype=linetype)
	else
		q<-q + geom_errorbar(aes_string(ymax = paste(y,"+",err), ymin=paste(y,"-",err)),width=width,color=color,linetype=linetype)
	return(q)
}

# scales
plot.addAxes<-function(q, xlim, xbreaks, ylim, ybreaks) {
	if(length(xlim)==2 && length(ylim)==2) { 
		q<-q+coord_cartesian(xlim=xlim, ylim=ylim)
		q<-q+scale_y_continuous(breaks=ybreaks)+scale_x_continuous(breaks=xbreaks)
	} else if(length(ylim)==2) {
		q<-q+coord_cartesian(ylim=ylim)
		q<-q+scale_y_continuous(breaks=ybreaks)
	} else if (length(xlim)==2) {
		q<-q+coord_cartesian(xlim=xlim)
		q<-q+scale_x_continuous(breaks=xbreaks)
	}
	return (q)	
}

# simple background
plot.addSimpleBgrd<-function(q)
	return (q + opts(panel.background = theme_rect(colour = 'black', size = 1.5, linetype='solid'), panel.grid.major = theme_line(colour = 'gray', size = 0.5, linetype = 'solid')))

# standard labels
plot.addLabels<-function(q,main,xlab,ylab,colLegend=NULL,shapeLegend=NULL, lineLegend=NULL, fillLegend=NULL) 
	return (q + opts(title = main) + labs(y = ylab, x = xlab, colour=colLegend, shape=shapeLegend, linetype=lineLegend, fill=fillLegend))

########################
# complex graphs funcs #
########################

# q = a ggplot object which has 
#	- its colour aesthetic set (even if it's =factor(ID) and all colours passed in the colours argument are the same)
# 	- its size aesthetic set to the same distinguisher as the color aesthestic (e.g. some ID, use factor(ID) if discrete)
#	- no geom_line OR geom_line(size=x) called BEFORE geom_point() ;(size has to be fixed in geom_line attribute)
# colors = color_palette used, e.g. c("black", "blue", "red", "dark green")
# order = the order (relative to the colors provided) in which the graphs should appear, e.g. c(4,1,3,2)
# autosave = TRUE: save each plot automatically, FALSE: ask whether to save it
# filename = base filename (number in the animation sequence is added at the end), omit ".pdf"
# ... = additional arguments are all passed on to ggsave (e.g. width, height, dpi)
ggplot.animate<-function(q, order, colors, psize=2.5, autosave=FALSE, filename="animate", ...){
	if (max(order)>length(colors) || min(order) < 1) {
		print("ERROR: order includes indices that are not available. If you provide 4 colours, only provide order numbers 1 through 4, e.g. c(1,4,2,3).")
		return()
	}
	animateSizes<-rep(0,times=length(colors))
	animateColors<-rep("white", times=length(colors))
	for(i in 0:length(order)) {
		if (i>0){
			animateSizes[order[i]]<-psize
			animateColors[order[i]]<-colors[order[i]]
		}
		print(q + scale_size_manual(values=animateSizes, legend=FALSE) + scale_colour_manual(values=animateColors))
		if (autosave==TRUE) {
			save <- "y"
		} else {
			save <- ask(paste("Would you like to save this plot (#", i, ")? Type y for yes, enter for no.", sep=""))
		}
		if (save=="y") {
			name<-paste(filename,"_",i,".pdf",sep="")
			print(paste("Saving plot #", i, " as ", name, sep=""))
			ggsave(filename=name,...)
		}
	}
}

##### BORROWED MULTIPLOT FUCTION #####
# from R cookbook
# Multiple plot function
# NOTE: the layout structure is extremely useful!!
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



####################
# simplicity funcs #
# for + syntax use #
# TODO: figure out how to use multiple ggplot commands (i.e. the + syntax)
# TODO: upgrade to new ggplo2 syntax (a lot of commands are now deprecated)
####################


# make black and white theme (call before calling any other theme vars, sucha s nogrid, adjustX, etc.)
gg.bw<-function() theme_bw()

# removing grid (call after gg.bw, otherwise gg.bw recreates the lines)
gg.nogrid<-function() return(opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank()))

# add title
gg.title<-function(title) opts(title=title)

# adjust x axis position
gg.adjustX<-function(vjust=-0.5, hjust=0, size=12) opts(axis.title.x = theme_text(vjust=vjust, hjust=0.5+hjust, size=size))

# adjust y axis position (note to self: maybe allow adjusting face="bold"?)
gg.adjustY<-function(vjust=0.2, hjust=0, size=12, angle=90) opts(axis.title.y = theme_text(vjust=vjust, hjust=0.5+hjust, angle=angle, size=size))

# change margins
# recommmend using this together with gg.noLegend, otherwise need to adjust the right parameter substantially
gg.noMargins<-function(ylabel=TRUE, xlabel=TRUE, top=0.2, right=-0.5, bottom=-1, left=-1) {
	if (ylabel==TRUE) left<-left+1
	if (xlabel==TRUE) bottom<-bottom+1
	opts(plot.margin = unit(c(top,right,bottom,left), "lines"))
}

# remove legend
gg.noLegend<-function() opts(legend.position="none")

