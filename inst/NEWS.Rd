\name{NEWS}
\title{blackbox News}
\encoding{UTF-8}
\section{CHANGES UP TO VERSION 1.0.6}{
\subsection{BUGS}{
\itemize{
\item (Migraine-specific) Calls to Rf_error() removed from C++ code.
\item (Migraine-specific) An invalid cbind() that affected the generation of 'nextpoints' has been corrected.
\item (Migraine-specific) Within subHullWrapper(), the scdd() results with double precision are now checked and replaced by a computation with rational arithmetic if they are inaccurate.  
\item (Migraine-specific) "samplingSpace=,Nb," has been broken for a long time. This is corrected.
\item (Migraine-specific) sampleNextPars() could stop when called with non-default 'samplingSpace'. This is corrected.
}
}
}


