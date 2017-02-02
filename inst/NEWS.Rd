\name{NEWS}
\title{blackbox News}
\encoding{UTF-8}
\section{CHANGES UP TO VERSION 1.0.15}{
\subsection{BUGS}{
\itemize{
\item More C++ portability issues fixed.
\item (Migraine-specific, without any impact on inferences) optimization convergence diagnostic (actually nloptr()'s return status) was missing from the output_...txt file, resulting in lines with different number of values.
\item (Migraine-specific) toCanonical() corrected to handle single-candidate input matrix.
\item (Migraine-specific) fixed parameter space issues in generateNewPoints() and canonize().
}
}
\subsection{USER-LEVEL CHANGES}{
\itemize{
\item Modified init values of some smoothing parameters may change bboptim() results for rough response surfaces.
}
}
\subsection{OTHER CHANGES}{
\itemize{
\item Some function definitions has been moved from the spaMM package to blackbox.
}
}
}
\section{CHANGES UP TO VERSION 1.0.8}{
\subsection{BUGS}{
\itemize{
\item (Migraine-specific) Wrong variable spaces in generateNewPoints() and toCanonical() corrected.
\item (Migraine-specific) Calls to Rf_error() removed from Rcpp code.
\item (Migraine-specific) An invalid cbind() that affected the generation of 'nextpoints' has been corrected.
\item (Migraine-specific) Within subHullWrapper(), the scdd() results with double precision are now checked and replaced by a computation with rational arithmetic if they are inaccurate.  
\item (Migraine-specific) "samplingSpace=,Nb," has been broken for a long time. This is corrected.
\item (Migraine-specific) sampleNextPars() could stop when called with non-default 'samplingSpace'. This is corrected.
}
}
}


