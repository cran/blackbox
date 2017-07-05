toTgmuFromCanonical <- function(parvec) { ##utility to convert parvec c(twoNmu,D,T, ..., twoNancmu) to c(TwoNmu,D,Tgmu ..., twoNancmu)
  if(class(parvec)=="data.frame" || class(parvec)=="list") parvec <- unlist(parvec) ## keep names
  return(c(Tgmu=parvec[["T"]]*parvec[["twoNmu"]]))
}
