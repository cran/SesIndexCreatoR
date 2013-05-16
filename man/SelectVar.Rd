\name{SelectVar}
\alias{SelectVar}

\title{
Internal function: Selection of variables
}
\description{
Internal function. Select, in a given set of variables, the variables with the highest correlation with the first component of an Principal Component Analysis (PCA) or a Multiple Factor Analysis (MFA).
}
\usage{
SelectVar(X, sup = NULL, varnames = NULL, nb = 1, method = "PCA")
}
\arguments{
  \item{X}{
A dataframe
}
  \item{sup}{
A vector. The names of the supplementary units. 
}
  \item{varnames}{
A vector or a list of vectors (see details). The names of the variables to include in the procedure (by default, all the numeric variables in X).
}
  \item{nb}{
An integer or \dQuote{mean}. The number of variables to select (see details).
}
  \item{method}{
A character string. By default \dQuote{PCA} and the selection is based on a Principal Component Analysis, else \dQuote{MFA} uses a Multiple Factor Analysis.
}
}
\details{
This function selects the variables with the highest correlation with the first component given by the chosen method. It is used in the function \code{\link{SesIndex}}.

In all the function, if some values are missing in a variable included in a part of the analysis, all the corresponding rows are dropped (using \code{\link{na.omit}}) for this part of the analysis. 

If \code{nb} is an integer, the selection will be the \code{nb} variables with the highest correlations with the first component. If \code{nb} is \dQuote{mean}, the selection will be all the variables with a correlation with the first component higher than the average correlation.

If a PCA is used, \code{varnames} can be either a vector or a list of variables names. If MFA is used, \code{varnames} should be a list where each element is a group of variables represented by a vector containing their names (if \code{varnames} is not a list when MFA is used then the result will be identical to PCA).
}
\value{
Returns a list including :
\item{analysis}{Detailed results of the analysis, PCA or MFA, such as results provided by \code{\link{PCA}} or \code{\link{MFA}}}
\item{selection}{Vector containing the names of the selected variables.}
}
\author{
Benoît Lalloué
}

\seealso{
\code{\link{SesIndex}}
}
	
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
