\name{ClassifInt}
\alias{ClassifInt}
\title{
Internal function: Classification by intervals
}
\description{
Internal function. Create categories by intervals, and export the results in a convenient way for \code{\link{SesClassif}} function.
}
\usage{
ClassifInt(x, nb)
}

\arguments{
  \item{x}{
A numeric vector.
}
  \item{nb}{
An integer. The number of categories to create. 
}
}

\details{
This function is an internal function of the package. It is used to create categories based on intervals and export the results in a convenient way for \code{\link{SesClassif}} function.

\code{nb} categories are created by dividing the range of \code{x} in equals intervals.
}

\value{
Returns a list containing:
\item{classes}{A vector containing the categories.}
\item{bounds}{The cut points.}
}

\author{
Benoît Lalloué
}


\seealso{
\code{\link{ClassifHC}}, \code{\link{ClassifQuant}}, \code{\link{SesIndex}}, \code{\link{SesClassif}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
