#######################################
# Code for isodat file parser (IDP)   #
# Calculations                        #
# Copyright 2013 Sebastian Kopf       #
# seb.kopf@gmail.com                  #
#######################################

# FUNCTION: utility functions for converstion between dD and FD (in multi isotope system, pass all ratios! e.g. c(18O/16O, 17O/16O))
iso.RtoF<-function(ratio) ratio/(1+ratio) # ratio to fraction
iso.FtoR<-function(frac) frac/(1-frac) # fraction to ratio
iso.RtoD<-function(ratios, refs) ratios/refs - 1 #fraction to delta value (NOT 1000x!!)
iso.RtoDx<-function(ratios, refs) iso.RtoD(ratios, refs)*1000 #fraction to delta value (1000x)
iso.DtoR<-function(deltas, refs) (deltas+1) * refs #delta to fraction (delta NOT 1000x!!)
iso.DxtoR<-function(deltas, refs) iso.DtoR(deltas/1000, refs) #delta to fraction (deltas are 1000x)
iso.FtoD<-function(fracs, refs) iso.RtoD(iso.FtoR(fracs), refs) #fractions to delta values (deltas NOT 1000x)
iso.FtoDx<-function(fracs, refs) iso.FtoD(fracs, refs)*1000 #fractions to delta values  (deltas are 1000x)
iso.DtoF<-function(deltas, refs) iso.RtoF(iso.DtoR(deltas, refs)) #delta values to fractions (deltas NOT 1000x)
iso.DxtoF<-function(deltas, refs) iso.DtoF(deltas/1000, refs) #delta values to fractions (deltas are 1000x)
iso.DtoA<-function(deltas1, deltas2) (deltas1 + 1)/(deltas2 + 1) #delta notation to alpha fractionation factor (not 1000X)
iso.DxtoA<-function(deltas1, deltas2) iso.DtoA(deltas1/1000, deltas2/1000) #delta (in permil, i.e. 1000x) to alpha
iso.AtoEx<-function(alphas) (alphas-1)*1000 #alpha to epsilon (as permill, i.e. 1000x)
iso.DxtoEx<-function(deltas1, deltas2) iso.AtoEx(iso.DxtoA(deltas1, deltas2)) # from delta notation (in permil, i.e. 1000x) to epsilon in permil
