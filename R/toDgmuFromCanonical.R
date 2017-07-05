toDgmuFromCanonical <- function(parvec) { ##utility to convert parvec c(twoNmu, D,T,..., twoNancmu) to c(TwoNmu,Dgmu,T, ..., twoNancmu)
  if(class(parvec)=="data.frame" || class(parvec)=="list") parvec <- unlist(parvec) ## keep names
  return(c(Dgmu=parvec[["D"]]*parvec[["twoNmu"]]))
}
