\name{SesClassif}
\alias{SesClassif}
\title{
Create categories from a socioeconomic index
}
\description{
Create socioeconomic categories based on a socioeconomic index created by \code{\link{SesIndex}} function with different technics.
}
\usage{
SesClassif(X, nb = "auto", method = "HC+knn")
}
\arguments{
  \item{X}{
An object resulting from the \code{\link{SesIndex}} function.
}
  \item{nb}{
An integer or \dQuote{auto}. The number of categories to create. If \dQuote{auto} the number is automatically determined (see details).
}
  \item{method}{
A character string among \dQuote{HC}, \dQuote{HC+knn}, \dQuote{quantiles} or \dQuote{interval}. The method used to create the categories (see details). 
}
}
\details{
This function creates socioeconomic categories based on a socioeconomic index created by the \code{\link{SesIndex}} function.

Method \dQuote{HC} creates categories with a Hierarchical Clustering on the 10 first components of the final PCA used to create the index. Supplementary units are not included and will not have a category.
Method \dQuote{HC+knn} proceeds first like \dQuote{HC} but then performs a k-nearest neighbor algorithm (with k=10) in order to classify and give a category to the supplementary units (training set is then the set of active units and test set the set of supplementary units). Others units will have the same category than with \dQuote{HC} method.
Method \dQuote{quantile} creates categories using quantiles of the index distribution (and then uses only the first component of the final PCA of the creation procedure).
Method \dQuote{interval} creates categories using equal subdivisions of the range of the index distribution (and then uses only the first component of the final PCA of the creation procedure).

If \code{nb} is set to \dQuote{auto}, the number of categories is automatically defined for methods \dQuote{HC} and \dQuote{HC+knn} by cutting the tree where the relative loss of inertia is the highest. For methods \dQuote{quantiles} and \dQuote{interval}, \dQuote{auto} defines the number of classes to 4.
}
\value{
Returns a list containing:
\item{table}{A dataframe containing the original dataset and a new column with the categories}
\item{analysis}{If \code{method} is \dQuote{HC} or \dQuote{HC+knn}, the results of the HC such as results provided by \code{\link{HCPC}}. If \code{method} is \dQuote{quantiles} or \dQuote{intervals}, the values of the cut points.}
\item{call}{Arguments passed in the call of the function.}
}


\references{
Lalloué et al.(2013) A statistical procedure to create a neighborhood socioeconomic index for health inequalities analysis. \emph{International Journal for Equity in Health} \bold{12},21
}

\author{
Benoît Lalloué
}

\seealso{
\code{\link{SesIndex}}, \code{\link{HCPC}}, \code{\link{knn}}, \code{\link{print.SesClassif}}, \code{\link{plot.SesClassif}}, \code{\link{SesReport}}
}
\examples{
# Importation of the table: column name are on the first row, 
# units name are on the first column, 
# column are separated by a tabulation
SesData <- read.table(system.file("extdata","SesData.txt", package = "SesIndexCreatoR"),
                      header=TRUE,sep="\t", row.names=1)

# Creation of the socioeconomic index with SesIndex
varnames <- colnames(SesData)[2:ncol(SesData)]
group1 <- grep("+Unemployed", colnames(SesData), value=TRUE)
group2 <- grep("+LabourForce", colnames(SesData), value=TRUE)
groupvarnames <- list(group1, group2)
illus <- rownames(SesData[SesData[,"Type"] \%in\% c("A", "D"),])
index <- SesIndex(SesData, varnames=varnames, groupvarnames=groupvarnames, sup=illus)

# Classification with default options: 
# automatic number of classes, Hierarchical Clustering and k nearest neighbors
classif1 <- SesClassif(index)
plot(classif1)

# Classification in 5 classes with quantiles
classif2 <- SesClassif(index, nb=5, method="quantiles")
plot(classif2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{cluster}
