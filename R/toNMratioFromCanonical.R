toNMratioFromCanonical <- function(parvec) { ##utility to convert parvec c(twoNmu,Q1,M1,M2,...) to c(twoNmu,Q1,NMratio,M2,...)
  if(class(parvec)=="data.frame" || class(parvec)=="list") parvec <- unlist(parvec) ## keep names
  return(c(NMratio=parvec[["M1"]]/parvec[["M2"]]))
}
