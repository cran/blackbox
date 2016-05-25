overmessage <- function(msg, prevmsglength) {
  msglength <- nchar(msg)
  if (prevmsglength>0) {base::message("\r", appendLF=F)}  	##FR: for backslash-b see ?Quotes ...
  base::message(msg, appendLF=F)
  return(msglength)
}
