# GUI utility functions
guiQuit <- function(module, hub) {
  ## FIXME: implement proper shutdown triggered over modules and hub to ask for saving settings
  # get out of modal loop, etc.
  dispose(module$widgets$win)
}