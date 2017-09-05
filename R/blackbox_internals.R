.overcat <- function(msg, prevmsglength) {
  if (prevmsglength>0) {cat("\r")}    
  cat(msg)
  return(nchar(msg))
}

.ULI <- function(...) {  ## Unique Location Index; '...' are simply names of variables in the data
  ## the behaviour of unique(<several columns>) is to compare character representations
  ## ... but this is not the default behaviour if a single column
  ## => we need to standardize this 
  ## If this is modified then the computation of uniqueGeo in HLCor needs to be modified 
  redondGeo <- cbind(...) ## always a matrix
  redondFac <- apply(redondGeo,1,paste,collapse=" ") ## always characters whatever the number of columns 
  redondFac <- as.integer(as.factor(redondFac)) ## as.factor effectively distinguishes unique character strings 
  uniqueFac <- unique(redondFac) ## seems to preserve order ## unique(<integer>) has unambiguous behaviour
  sapply(redondFac, function(v) { which(v==uniqueFac) })
}