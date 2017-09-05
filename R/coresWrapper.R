.check_nb_cores <- function(nb_cores=NULL) {
  if (is.null(nb_cores)) nb_cores <- blackbox.getOption("coreNbr") ## may be NULL
  machine_cores <- parallel::detectCores()
  if (is.null(nb_cores)) {
    nb_cores <- 1L ## default
    if (machine_cores>1L && interactive()) {
      if (! identical(blackbox.getOption("cores_avail_warned"),TRUE)) {
        
        message(paste(machine_cores,"cores are available for parallel computation\n(you may be allowed to fewer of them on a shared cluster).\n"))
        if ("Migraine" %in% blackbox.getOption("usedBy")) {
          message("Use 'CoreNumberForR' keyword in Migraine settings file to set the number of cores to be used by Migraine during the R blackbox analysis.\n Or use blackbox.options(coreNbr=<n>) to control coreNbr during the execution of the R scrpt.")
        } else {
          message("Change 'coreNbr' argument to use some of them.\nUse blackbox.options(coreNbr=<n>) to control coreNbr globally.")
        }
        blackbox.options(cores_avail_warned=TRUE)
      }
    }
  } else if (nb_cores>machine_cores) {
        if (! identical(blackbox.getOption("nb_cores_warned"),TRUE)) {
          if ("Migraine" %in% blackbox.getOption("usedBy")) {
            warning("More cores were requested than found by parallel::detectCores(). Check 'CoreNumberForR' keyword in Migraine settings file.
                     Number of availlable cores automatically downsized to the number of cores found by parallel::detectCores(). I continue.")
          } else {
            warning("More cores were requested than found by parallel::detectCores(). Check blackbox.getOption(\"coreNbr\") argument.
                    Number of availlable cores automatically downsized to the number of cores found by parallel::detectCores(). I continue.")
          } 
          blackbox.options(nb_cores_warned=TRUE)
        }
    nb_cores <- machine_cores
  }
  return(nb_cores)
}

.init_cores <- function(nb_cores=NULL, ## passing explicit value from user
                        ...) {  ## ... are arguments used by functions called by the loc_calc_logL function
  nb_cores <- .check_nb_cores(nb_cores=nb_cores)
  if (nb_cores > 1L) {
    cl <- parallel::makeCluster(nb_cores) 
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    if (has_doSNOW <- ("doSNOW" %in% .packages() )) { ## allows progressbar but then requires foreach
      # loading (?) the namespace of 'snow' changes the global RNG state!
      assign(".Random.seed", R.seed, envir = .GlobalEnv)
      eval(as.call(c(quote(registerDoSNOW),list(cl=cl)))) 
    } else if ( ! identical(blackbox.getOption("doSNOW_warned"),TRUE)) {
      message("If the 'doSNOW' package were attached, the progress of the computation could be reported.")
      blackbox.options(doSNOW_warned=TRUE)
    } 
    dotenv <- list2env(list(...))
    parallel::clusterExport(cl=cl, as.list(ls(dotenv)),envir=dotenv) 
    return(list(cl=cl,nb_cores=nb_cores,has_doSNOW=has_doSNOW))
  } else return(list(nb_cores=1L,has_doSNOW=NA,cl=NA))
  
}

.run_cores <- function(loc_calc_logL, pargrid, profileMethod, cores_info) {
  ii <- 0 ## 'global definition' (!)
  prevmsglength <- 0
  if (cores_info$nb_cores > 1L) {
    blackboxOptions <- blackbox.options()
    if (cores_info$has_doSNOW) {
      pb <- txtProgressBar(max = nrow(pargrid), style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress = progress)
      parallel::clusterExport(cl=cores_info$cl, list("progress"),envir=environment()) 
      `%foreachdopar%` <- foreach::`%dopar%`
      grid_obj <- foreach::foreach(
        ii = 1:nrow(pargrid),
        .combine = "rbind",
        .inorder = TRUE,
        .packages = "blackbox",
        .errorhandling = "remove",
        .options.snow = opts
      ) %foreachdopar% {
        loc_calc_logL(pargrid[ii,], profileMethod, blackboxOptions)
      }
      #browser() # tres utile pour voir les sorties des calculs en parallele, débugger avec des return a chaque point de blocage, et pour voir l'avancement
      close(pb)
    } else {
      parallel::clusterEvalQ(cores_info$cl, {library(blackbox)}) 
      grid_obj_list <- parallel::parApply(cores_info$cl,pargrid,MARGIN = 1L, FUN = loc_calc_logL, profileMethod, blackboxOptions)
      for ( i in 1:nrow(pargrid) ) {
        vec_obj <- vector(mode = "list",length(grid_obj_list[[1]]))
        names(vec_obj) <- names(grid_obj_list[[1]])
        for ( j in 1:length(grid_obj_list[[1]]) ) vec_obj[j] <- list(grid_obj_list[[i]][[j]])
        if ( i==1) {grid_obj=vec_obj} else {grid_obj=rbind(grid_obj,vec_obj)}
      }
      #browser() # tres utile pour voir les sorties des calculs en parallele, débugger avec des return a chaque point de blocage, et pour voir l'avancement
    }
  } else {
    eval_wrap <- function(v,arg) {
      res <- loc_calc_logL(v,arg)
      ii <<- ii+1
      if (interactive()) {
        for (bidon in seq_len(prevmsglength)) cat("\b")
        msg <- paste(ii,"simulations run out of", NROW(pargrid)," ")
        prevmsglength <<- .overcat(msg, prevmsglength)
      } else {
        cat(ii);cat(" ")
        if ((ii %% 40)==0L) cat("\n")
      }
      return(res)
    }
    grid_obj <- apply(pargrid,MARGIN = 1L,FUN = eval_wrap,profileMethod,NULL)
    grid_obj <- t(grid_obj)
  }
  
  return(grid_obj)
}