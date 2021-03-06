.blackbox.data <- new.env(parent = emptyenv())
.blackbox.data$Constants <- list(Version = "debug") ## if the user sees this, it's because the sources have been read interactively...
.blackbox.data$options <- list(FONKgScale=NULL,
                               FONKgNames=NULL,
                               CIlevel=0.05, ## requested by calc1DCIs
                               miscOptions=c("optimizeKriging", "ignoreSmallEigen"), ## OK for clean mode
                               minSmoothness=0.001,
                               maxSmoothness=8,
                               samplingSpace=NULL,
                               samplingScale=NULL,
                               plotRange=NULL,
                               ##  FR->FR For reset while reading migraine.R, NULL is a priori not sufficient
                               extraScale=c(),
                               LikStatistic=NULL,
                               lambdaEst = NA, ## cf default values expected by C...
                               #hglmLambdaEst = NA,
                               #hglmPhiEst = NA,
                               CIlo = NA,
                               CIup = NA,
                               dump_frames = FALSE, ## private option (initially) for debugging rbb()
                               ycolname="-ln(L)", ## the response for Kriging
                               optimizers=NULL,
                               ## on a besoin des trucs explicitement modifiés par onLoad ici en car de source() manuel
                               interactiveGraphics=TRUE, ## if the user sees this, it's because the sources have been read interactively...
                               stdoutRedirBool=FALSE,
                               verbosity=0,
                               useEI=TRUE,
                               memcheck=-1, ## >0: threshold for browser() => no check when memcheck<=0;
                               cores_avail_warned=FALSE,
                               nb_cores_warned=FALSE,
                               coreNbr=NULL,
                               ##### used by many fns even outside Migraine
                               ParameterNames=NULL,
                               ##### not documented bc used in the body of undocumented Migraine fns
                               graphicsFormat="eps", #providePlotFile
                               plotOptions=NULL, ## FR->FR attention min/maj dans code C->R  !!!!!!!!!!!!!!!!!!!!
                               graphicPars=NULL,
                               ##### not documented bc used only as controls of undocumented fns (dangerous)
                               max_mat_size=1e7,
                               # ...
                               ##### not documented bc used only as arguments (not default) of documented fns (OK)
                               designRetain=1, ## default in and outside Migraine
                               GCVdesignRetain=1, ## default in and outside Migraine
                               ##### also used internally:
                               redundant.mode=NULL, ## OK for clean code
                               example_maxtime=2,
                               # plotFiles
                               # hulls
                               # spaMM controls
                               control_spaMM=list(fix_predVar=NA) ## =TRUE would call gmp on large matrix... hmmm
)

blackbox.options <- function(...) {
  if (nargs() == 0) return(.blackbox.data$options)
  temp <- list(...)
  if (length(temp) == 1 && is.null(names(temp))) {
    arg <- temp[[1]]
    switch(mode(arg),
           list = temp <- arg,
           character = return(.blackbox.data$options[arg]),  ## return here for eg ... = "NUMAX"
           stop("invalid argument: ", sQuote(arg)))
  }
  if (length(temp) == 0) return(.blackbox.data$options)
  argnames <- names(temp)
  if (is.null(argnames)) stop("options must be given by name")
  old <- .blackbox.data$options[argnames]
  names(old) <- argnames ## bc names are not valid for previously absent elements
  .blackbox.data$options[argnames] <- temp
  invisible(old)
}

blackbox.getOption <- function (x) {blackbox.options(x)[[1]]}


".onAttach" <- function (lib, pkg) {
  version <- utils::packageVersion("blackbox")
  packageStartupMessage("blackbox (version ", version,
                        ") is loaded.",
                        "\nType 'help(blackbox)' for a short introduction.")
}


".onLoad" <- function (lib, pkg) {
  .blackbox.data$options$interactiveGraphics <- (interactive() && ((.Platform$OS.type=="windows") || capabilities("X11")))
  message <- message.redef
  tmp <- commandArgs()
  if (stdoutRedirBool <- any(tmp=="--redirOut")) { ## then we redef stop.redef() so that messages go both to screen and to output file
    stop <- stop.redef
  }
  .blackbox.data$options$stdoutRedirBool <- stdoutRedirBool
  d <- c(-1,1)
  abyss <- suppressMessages(delaunayn( as.matrix(rbind(expand.grid(d,d,d),0)))) # *sigh*
}

".onUnload" <- function (libpath) {
  library.dynam.unload("blackbox", libpath)
}
#  pkgpath <- system.file(package="blackbox") # https://github.com/hadley/devtools/issues/119
# library.dynam.unload("Rmixmod",libpath=system.file(package="Rmixmod"))... correct syntax, no effect on memory
