Module <- setRefClass(
  'Module', 
  fields = list(params = 'list', standalone = 'logical'), 
  methods = list(
     initialize = function(...){
       callSuper(...)
       standalone <<- FALSE # by default connected to a hub
       params <<- list(a=1:5)
     },
     speak = function() cat("\n i am", class(.self)),
     showGUI = function(hub) {
       if (!is(hub, "Hub")) stop("module needs a hub to ")
       speak()
       cat("\n show gui")
     },
     addParams = function(...){
      params <<- modifyList(params, list(...))
    }
  )
)

is(DataManager$new(), "Module")