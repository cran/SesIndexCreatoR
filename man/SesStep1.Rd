\name{SesStep1}
\alias{SesStep1}
\title{
Internal function: perform the first step of the creation of the socioeconomic index
}
\description{
Internal function. Perform the first step of the creation of the socioeconomic index called by \code{\link{SesIndex}}.
}
\usage{
SesStep1(X, varnames, groupvarnames, sup)
}
\arguments{
  \item{X}{
A dataframe containing the data.
}
  \item{varnames}{
A vector or a list of vectors (see details). The names of the variables to include in the procedure.
}
  \item{groupvarnames}{
A list of vectors. Each vector containing the names of the variables of a redundant group.
}
  \item{sup}{
A vector. The names of the supplementary units. 
}
}

\details{
This function perform the first step of the creation of the socioeconomic index called by \code{\link{SesIndex}}.

For each element of \code{groupvarnames} (each vector of variables names) representing a redundant group of variable, a PCA is performed and only the variable with the highest correlation with the first component is kept.
}
\value{
Returns a list including:
\item{analysis}{Detailed results of the PCA on each redundant group, such as results provided by \code{\link{PCA}}.}
\item{selection}{Vector containing the names of the variable selected in each group.}
}

\author{
Benoît Lalloué
}

\seealso{
\code{\link{PCA}}, \code{\link{SesIndex}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
