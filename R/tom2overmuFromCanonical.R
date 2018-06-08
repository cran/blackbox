tom2overmuFromCanonical <- function(parvec) { ##utility to convert parvec c(twoNmu,Q1,M1,M2,...) to c(TwoNmu,Q1,M1,m2overmu,...)
  if(class(parvec)=="data.frame" || class(parvec)=="list") parvec <- unlist(parvec) ## keep names
  return(c(m2overmu=parvec[["M2"]]/parvec[["twoNmu"]]/(1.0-parvec[["Q1"]])))
}
