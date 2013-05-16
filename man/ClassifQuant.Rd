\name{ClassifQuant}
\alias{ClassifQuant}
\title{
Internal function: Classification by quantiles
}
\description{
Internal function. Create categories by quantiles, and export the results in a convenient way for \code{\link{SesClassif}} function.
}
\usage{
ClassifQuant(x, nb)
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
This function is a internal function of the package. It is used to create categories based on quantiles and export the results in a convenient way for \code{\link{SesClassif}} function.

\code{nb} categories with equals effectives are created using quantiles.
}
\value{
Returns a list containing:
\item{classes}{A vector containing the categories.}
\item{bounds}{The cut points.}
}

\author{
%%  ~~who you are~~
Benoît Lalloué
}

\seealso{
\code{\link{ClassifHC}}, \code{\link{ClassifInt}}, \code{\link{SesIndex}}, \code{\link{SesClassif}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
