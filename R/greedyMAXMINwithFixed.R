greedyMAXMINwithFixed <- function(rownamedarray, finallength, scales, 
                                  fixedNbr=NA ## numeric: number of final rows of rownamedarray, that should be retained in result
                                  ) { # return rownames!
  ## the fixed points should be the last fixedNbr rows in rownamedarray !!
  ## derived from greedy MAXMIN algo, Ravi et al 1991 in Rennen 2008
  #finallength includes fixed points
  if (is.na(fixedNbr)) {
    stop.redef("(!) from greedyMAXMINwithFixed : at least one fixed point required")
    ## peut etre lui faire chercher les deux points les plus distants sans stocker la matrice en memoire
  } else if (fixedNbr<1L) {
    stop("fixedNbr must be a positive integer")
  } ## otherwise greedyMAXMINwithFixed would need to compute the matrix of distances between all points
  if (fixedNbr==finallength) {
    return(rownames(rownamedarray))
  } ## else :
  if (nrow(rownamedarray)<=finallength) {
    if (nrow(rownamedarray)<finallength)
      cat("(!) In greedyMAXMINwithFixed(...): size of input array lower than target final size", "\n")
    return(rownames(rownamedarray))
  } ## else :
  kriglength <- blackbox.getOption("kriglength") ## may be NULL
  if ( ( ! is.null(kriglength)) && (nrow(rownamedarray)-finallength)*finallength > kriglength**2) { ## cf size of final distance matrix
    message.redef("(!) From greedyMAXMINwithFixed: will be slow, and may not have enough memory... ")
  }
  localarray <- sweep(rownamedarray,2L,sqrt(scales),FUN=`/`) # t(t(rownamedarray)/sqrt(scales))
  nnr <- nrow(localarray)
  nnf <- nnr-fixedNbr
  subsetids <- (nnf+1):nnr # indices of rows to be returned, initialized by indices of fixed points (which are last rows of the array).
  fixedpts <- localarray[subsetids, , drop=FALSE]
  candidates <- localarray[ - subsetids, , drop=FALSE]
  ##now we compute the distance of all subset points with all nonsubset points
  ##(but not non-subset with non-subset)
  distCS <- proxy::dist(candidates,fixedpts) # crossdist object, similar to matrix, [ncandidates, nfixed]
  rowmins <- matrixStats::rowMins(distCS) # vector [ncandidates]
  needed <- finallength-fixedNbr
  subsetids <- c(rep(NA_integer_,needed),subsetids) # will store indices of rows to be returned
  for (it in seq_len(needed)) {
    # Find best point in terms of maxmin distance:
    farthestidx <- which.max(rowmins) 
    subsetids[it] <- farthestidx 
    # Generate new 'rowmins':
    farthest_candidate <- candidates[farthestidx, , drop=FALSE]
    newdist <- proxy::dist(candidates, farthest_candidate) ## 1col crossdist object, includes compar of farthest_candidate with itself => O element
    rowmins <- pmin(rowmins,newdist) # => element 0 for candidates now included in the retained set
    # We might removes the rows of retained points from 'candidates' (very costly) and vectors that refer to it, such as 'rowmins'
    # It's faster to let the zero values in 'rowmins'.  
  }
  return(rownames(localarray)[subsetids])  ##includes fixed points
}
