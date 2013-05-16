\name{ClassifHC}
\alias{ClassifHC}
\title{
Internal function: Classification with Hierarchical Clustering (HC)
}
\description{
Internal function. Create categories with HC and k-nearest neighbors, based on a PCA object, and export the results in a convenient way for \code{\link{SesClassif}} function.
}

\usage{
ClassifHC(X, nb, knn = FALSE)
}

\arguments{
  \item{X}{
An object resulting of the \code{\link{PCA}} function
}
  \item{nb}{
An integer or -1. The number of categories to create. If -1 the number is automatically determined (see details).
}
  \item{knn}{
A boolean. Use the k-nearest neighbor method after HC or not.
}
}
\details{
This function is an internal function of the package. It is used to create categories based on a PCA object created by the \code{\link{PCA}} function and export the results in a convenient way for \code{\link{SesClassif}} function.

If \code{nb} is set to -1, the number of categories is automatically defined by cutting the tree where the relative loss of inertia is the highest.

The function creates categories with a Hierarchical Clustering on the 10 first components of \code{X}. 
If \code{knn} is FALSE, supplementary units are not included and will not have a category.
If \code{knn} is TRUE, the function proceeds first a Hierarchical Clustering and then performs a k-nearest neighbor algorithm (with k=10) in order to classify and give a category to the supplementary units (training set is then the set of active units and test set the set of supplementary units). Others units will have the same category than with \code{knn} set to FALSE.
}

\value{
Returns a list containing:
\item{classes}{A vector containing the categories.}
\item{analysis}{The results of the HC such as results provided by \code{\link{HCPC}}.}
}

\author{
Benoît Lalloué
}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{ClassifInt}}, \code{\link{ClassifQuant}}, \code{\link{SesIndex}}, \code{\link{SesClassif}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
