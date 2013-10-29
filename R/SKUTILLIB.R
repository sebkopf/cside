#####################################
# Utility functions  			        	#
# Copyright 2013 by Sebastian Kopf  #
#####################################

######################
# variable functions #
######################

# get all variables/functions/everything defined in the passed in environment as a list with the variable name = variable object
# e.g. to get a list of all variables in a function, just call ls.asList(environment()) from within the function
# params
#  env = the environment to get do ls() on
#  exclude - a vectr of variable/function names to exclude from the list
ls.asList<-function(env, exclude=c()) sapply(setdiff(ls(env=env), exclude), FUN=function(i) list(get(i, env=env)))

#############################
# small scale utility funcs #
#############################

# checks if lenght of the variable is 0 and returns TRUE if it is
# also returns TRUE if variable is NA or NULL
is.empty<-function(variable)
  return (is.null(variable) || is.na(variable) || length(variable) == 0)

# convert strings to date time
strToDateTime<-function(dateStr, timeStr, format="%m/%d/%y %H:%M"){
  if(!missing(timeStr)) dateStr<-paste(dateStr,timeStr)
  return(strptime(dateStr, format))
}

#timeDiff function
timeDiff<-function(dateTime, baseDT, units="days")   
  return (as.numeric(difftime(dateTime,rep(baseDT,length(dateTime))),units))
