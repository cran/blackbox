tomratioFromCanonical <- function(parvec) { ##utility to convert parvec c(twoNmu,Q1,M1,M2,...) to c(twoNmu,Q1,mratio,M2,...)
  if(class(parvec)=="data.frame" || class(parvec)=="list") parvec <- unlist(parvec) ## keep names
  return(c(mratio=parvec[["M1"]]/parvec[["M2"]]*(1.0-parvec[["Q1"]])/parvec[["Q1"]]))
}
