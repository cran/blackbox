LowUpFromKnots <- function (knots,verbose=FALSE) { ## assumes knots are in samplingSpace, as LowUpfn does
  if (inherits(knots, "data.frame")) {
    localLow <- sapply(knots, min)
    localUp <- sapply(knots, max)
  } else {
    localLow <- matrixStats::colMins(knots, useNames=TRUE) # apply(knots, 2, min)
    localUp <- matrixStats::colMaxs(knots, useNames=TRUE) # apply(knots, 2, max)
  }
  return(LowUpfn(localLow, localUp, boundstype = "numerical",verbose=verbose))
}
