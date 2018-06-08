tom1overmuFromCanonical <- function(parvec) { ##utility to convert parvec c(twoNmu,Q1,M1,M1) to c(TwoNmu,Q1,m1overmun,M2)
  if(class(parvec)=="data.frame" || class(parvec)=="list") parvec <- unlist(parvec) ## keep names
  return(c(m1overmu=parvec[["M1"]]/parvec[["twoNmu"]]/parvec[["Q1"]]))
}
