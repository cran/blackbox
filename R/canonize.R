## wrapper around canonize, accepting matrix input and extracting canonVP 
## input has dim. Since 2016/01/05, output always has dim
toCanonical <- function(candidates, FONKgLow,
                        otherlist=NULL ## for completion from CI info 
                        ) {
  INFO <- blackbox.options()[c("ParameterNames","FONKgNames")]
  if(nrow(candidates)>1) {
    candidates <- apply(candidates, 1, tofullKrigingspace,fixedlist=otherlist)
    if (length(INFO$FONKgNames)>1L) {
      candidates <- t(candidates)
    } else candidates <- matrix(candidates, ncol=1)
    ## apply loses $canonVP names and instead copies the candidates' names...
    colnames(candidates) <- INFO$FONKgNames
    candidates <- apply(candidates, 1, function(v) {canonizeFromKrig(v)$canonVP}) ## transposed // expected; except if fittedparamnbr==1...
    if (length(INFO$ParameterNames)>1L) {
      candidates <- t(candidates)
    } else candidates <- matrix(candidates, ncol=1)
    ## apply loses $canonVP names and instead copies the candidates' names...
    colnames(candidates) <- INFO$ParameterNames
  } else {
    candidates <- tofullKrigingspace(candidates,fixedlist=otherlist)
    candidates <- t(canonizeFromKrig(candidates)$canonVP) ## t() converts to matrix with the column names
  }
  return(candidates)
}



canonizeFromKrig <- function(input) { ## from vector within Kriging space AND in Kriging scale, also returns composite var
  INFO <- blackbox.options()[c("FONKgLow","ParameterNames","DemographicModel","FONKgNames","FONKgScale")]
  FONKinput <- INFO$FONKgLow ## initial value
  if (length(setdiff(names(input),names(FONKinput)))>0L) {
    stop("debugging 29/04/2106: input argument of canonizeFromKrig() is invalid (should be within kriging space).")
  }
  FONKinput[names(input)] <- input
  FONKinput <- unlist(FONKinput) ## previous line sometimes creates a list although no argument is ??
  ## unlogs
  logs <- tolower(INFO$FONKgScale)=="logscale" ## vector of T/F
  FONKinput[logs] <- exp(FONKinput[logs]) ## argument of from2Ns2Tocanon should not be logscale....
  canon <- FONKinput
  names(canon) <-INFO$ParameterNames ## anticipate future names
  DemographicModel <- INFO$DemographicModel
  FONKgNames <- INFO$FONKgNames
  if ("IBD" %in% DemographicModel) {
    if("condS2" %in% FONKgNames) {
      canon["g"] <- groot(FONKinput["condS2"], D2bool= ("2D" %in% DemographicModel) )
    }
    if("latt2Ns2" %in% FONKgNames) {
      latt2Ns2 <- FONKinput["latt2Ns2"] ## saved in return value of canonizeFromKrig
      canon["twoNm"] <- from2Ns2Tocanon(FONKinput)["twoNm"]
    } else {## constructs 2Ds2 [lattice units]
      latt2Ns2 <- (tolatt2Ns2(canon))["latt2Ns2"] ## requires that canon is indeed already canonical
    }
    return(list(canonVP=canon, latt2Ns2=latt2Ns2))
  } else if ( length(intersect(DemographicModel, c("OnePopVarSize", "IM")))>0) {
    if("Nratio" %in% FONKgNames) {
      Nratio <- FONKinput["Nratio"] ## saved in return value of canonizeFromKrig
      canon["twoNmu"] <- FONKinput[["Nratio"]]*FONKinput[["twoNancmu"]]
    } else {## constructs Nratio
      Nratio <- toNratioFromCanonical(canon) ## requires that canon is indeed already canonical
    }
    return(list(canonVP=canon, Nratio=Nratio))
  } else if ("OnePopFounderFlush" %in% DemographicModel) {
    ##Remember that Nratio in FounderFush is called Nancratio for the user, RL 052013
    if("Nratio" %in% FONKgNames) {
      Nratio <- FONKinput["Nratio"] ## saved in return value of canonizeFromKrig
      canon["twoNmu"] <- FONKinput[["Nratio"]]*FONKinput[["twoNancmu"]]
    } else {## constructs Nratio
      Nratio <- toNratioFromCanonical(canon) ## requires that canon is indeed already canonical
    }
    if("NactNfounderratio" %in% FONKgNames) {
      NactNfounderratio <- FONKinput["NactNfounderratio"] ## saved in return value of canonizeFromKrig
      canon["twoNmu"] <- FONKinput[["NactNfounderratio"]]*FONKinput[["twoNfoundermu"]]
    } else {## constructs NactNfounderratio
      NactNfounderratio <- toNactNfounderratioFromCanonical(canon) ## requires that canon is indeed already canonical
    }
    if("NfounderNancratio" %in% FONKgNames) {
      NfounderNancratio <- FONKinput["NfounderNactratio"] ## saved in return value of canonizeFromKrig
      canon["twoNancmu"] <- FONKinput[["twoNfoundermu"]]/FONKinput[["NfounderNancratio"]]
    } else {## constructs NfounderNancratio
      NfounderNancratio <- toNfounderNancratioFromCanonical(canon) ## requires that canon is indeed already canonical
    }
    return(list(canonVP=canon, Nratio=Nratio, NactNfounderratio=NactNfounderratio, NfounderNancratio=NfounderNancratio))
  } else return(list(canonVP=canon))
} ## end def canonizeFromKrig
