.overcat <- function(msg, prevmsglength) {
  if (prevmsglength>0) {cat("\r")}    
  cat(msg)
  return(nchar(msg))
}

.ULI <- function(...) {
  redondGeo <- cbind(...) ## always a matrix
  if (ncol(redondGeo)==0L) return(rep(1L,nrow(redondGeo))) ## .generateInitPhi with constant predictor led here
  if (nrow(redondGeo)==1L) return(1L) ##  trivial case where the forllowingcode fails
  # redondFac <- apply(redondGeo,1,paste,collapse=" ") ## always characters whatever the number of columns 
  # redondFac <- as.integer(as.factor(redondFac)) ## as.factor effectively distinguishes unique character strings 
  # uniqueFac <- unique(redondFac) ## seems to preserve order ## unique(<integer>) has unambiguous behaviour
  # sapply(lapply(redondFac, `==`, uniqueFac ), which)
  redondFac <- apply(redondGeo,2L,factor,labels="") # not cute use of labels... 
  redondFac <- apply(redondFac,1L,paste,collapse="_") ## paste factors
  redondFac <- as.character(factor(redondFac))
  uniqueFac <- unique(redondFac) ## seems to preserve order ## unique(<integer>) has unambiguous behaviour
  uniqueIdx <- seq(length(uniqueFac))
  names(uniqueIdx) <- uniqueFac
  return(uniqueIdx[redondFac])
}

.inRstudio <- function(silent=FALSE, bool=TRUE) {
  trygetfn <- try(get("getActiveProject",envir = asNamespace("rstudioapi")), silent=silent) # fails if rstudioapi not installed
  if ( tryres <- ( ! inherits(trygetfn,"try-error"))) {
    tryres <- try(trygetfn(), silent=silent) # fails if rstudioapi not running ie not an Rstudio session
    # otherwise NULL if no active project, or the path if there is an active project.
  }
  if (bool) tryres <- ! inherits(tryres,"try-error") # to return a boolean : whether we are in an Rstudio session or not
  # otherwise retuen value is (try-error, or NULL, or path).
  tryres 
}

projpath <- local({
  pp <- NULL
  function() {
    if (is.null(ppp <- .blackbox.data$options$projpath)) {
      if (is.null(pp)) {
        projpathinRstudio <- .inRstudio(silent=FALSE, bool=FALSE)
        if (inherits(projpathinRstudio,"try-error") || is.null(projpathinRstudio)) { # not an Rstudio session || no active project
          if (interactive()) {
            message('Need to give the project path, say "C:/home/francois/travail/stats/blackboxplus/blackbox":')
            pp <<- readline(prompt="Enter path: ")
          } else {
            message('Need to start in the projpath, say "C:/home/francois/travail/stats/blackboxplus/blackbox", so that getwd() finds it.')
            pp <<- getwd()                  # would reach here in an R CMD check session (but in principle projpath() is never called in this context, 
            # except perhaps wrapped in a if (file_exists())
          }
        } else pp <<- projpathinRstudio
      }
      return(pp)
    } else return(ppp)
  }
})

