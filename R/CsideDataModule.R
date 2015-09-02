#' @include GuiModule.R
NULL

CsideDataModule = setRefClass(
  'CsideDataModule', contains='GuiModule', 
  fields = list(), 
  methods = list(
    initialize = function(...){
      callSuper(...);
    }
  ))