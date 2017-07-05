\name{options}
\alias{blackbox.options}
\alias{blackbox.getOption}
\title{blackbox options settings}
\description{
  Allow the user to examine a variety of \dQuote{options} (most of which are not true user options)
  which affect operations of the blackbox package.
}
\usage{
blackbox.options(\dots)

blackbox.getOption(x)

}
\arguments{
 \item{x}{a character string holding an option name.}
 \item{\dots}{A named value or a list of named values. Most are not to be manipulated by users and are undocumented. Exceptions are:
 \describe{
   \item{\code{ParameterNames}}{See Details}
   \item{\code{FONKgNames}}{See Details}
   \item{\code{fittedNames}}{See Details}
   \item{\code{gridStepsNbr}}{Number of steps of the grid of value for each parameter in profile plots.}
   \item{\code{graphicPars}}{Graphic parameters used for most plots.}
   \item{\code{verbosity=0}:}{Controls display of information about generalized cross-validation.
     \code{0} suppresses (most) messages.
     \code{1} displays information about estimates and progress of the procedure.
     Higher values display more information from the optimizer and possibly additional information.
   }
  }
 }
}
\details{
  blackbox.options() provides an interface for changing many undocumented options.

  The package is designed to infer likelihood surfaces by smoothing estimated likelihood points in a model with some canonical parameters (\bold{ParameterNames}). A transformed parameter space may be considered for smoothing, wherein some parameters are variable (\bold{fittedNames}) and others may be constant. The transformed parameter space including constant parameters has names \bold{FONKgNames} (FON for Fitted Or Not).
}
\value{
  For \code{blackbox.getOption}, the current value set for option \code{x}, or
  \code{NULL} if the option is unset.

  For \code{blackbox.options()}, a list of all set options.  For
  \code{blackbox.options(name)}, a list of length one containing the set value,
  or \code{NULL} if it is unset.  For uses setting one or more options,
  a list with the previous values of the options changed (returned
  invisibly).
}
\examples{
  blackbox.getOption("verbosity")
  \dontrun{
  blackbox.options(verbosity=1)
  blackbox.options()
  }
}
