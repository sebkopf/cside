#' Copies the variable to the clipboard
#' For data frames, writes as table (with separator tab), everything else directly.
copyToClipboard <- function(variable) {
  if (exists("writeClipboard")) # windows
    clipboard <- "clipboard"
  else # unix/MacOS
    clipboard <- pipe("pbcopy", "w")
  
  if (class(variable) == 'data.frame')
    write.table(variable, file=clipboard, sep="\t", row.names=FALSE)
  else
    cat(variable, file=clipboard)
  
  if (!exists("writeClipboard")) # unix
    close(clipboard)
}



testmeout<-function() {
  print("blablabl!")
}

showIcons <- function() {
gw<-gwindow("Icons")
grp<-ggroup(horizontal=FALSE, con=gw)
gicons<-ggroup(con=grp)
icons<-getStockIcons()
for (i in 1:length(icons)) {
  if (i%%8==0) gicons<-ggroup(con=grp)
  glabel(icons[i],con=gicons)
  gbutton(icons[i], con=gicons)
}
}


# rewrite for my purposes, from:
# http://stackoverflow.com/questions/7307987/logging-current-function-name

catw <- function(..., callstack=sys.calls()){
  cs <- callstack
  cs <- clean_cs(cs)
  #browser()
  message(paste(cs, ...))
}

clean_cs <- function(x){
  val <- sapply(x, function(xt){
    z <- strsplit(paste(xt, collapse="\t"), "\t")[[1]]
    switch(z[1],
           "lapply" = z[3], 
           "sapply" = z[3],
           "do.call" = z[2], 
           "function" = "FUN",
           "source" = "###",
           "eval.with.vis" = "###",
           z[1]
    )
  })
  val[grepl("\\<function\\>", val)] <- "FUN"
  val <- val[!grepl("(###|FUN)", val)]
  val <- head(val, -1)
  paste(val, collapse="|")
}
