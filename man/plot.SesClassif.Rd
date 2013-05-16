\name{plot.SesClassif}
\alias{plot.SesClassif}
\title{
Plot the results of the classification of a socioeconomic index
}
\description{
Plot the results of the classification of a socioeconomic index, with the different options provided by \code{\link{plot.PCA}} or \code{\link{plot.HCPC}}
}
\usage{
\method{plot}{SesClassif}(x, ...)
}
\arguments{
  \item{x}{
An object resulting from the \code{\link{SesClassif}} function. 
}
  \item{\dots}{
Further arguments passed to or from other methods (see details).
}
}
\details{
This function displays the projections of the units and their associated categories determined with the \code{\link{SesClassif}} function.
According to the method used in \code{SesClassif}, additional information are displayed: if the method is \dQuote{HC} or \dQuote{HC+knn}, the tree of the HC ; if the method is \dQuote{interval} or \dQuote{quantiles}, the cut points.

According to the method used in \code{SesClassif}, arguments for other functions can be passed in \code{\dots}: if the method is \dQuote{HC} or \dQuote{HC+knn}, arguments of \code{\link{plot.HCPC}} can be used ; if the method is \dQuote{interval} or \dQuote{quantiles}, \emph{some} arguments of \code{\link{plot.PCA}} can be used.
}
\value{
Returns the projections of the units and their associated categories.
Additional information: tree of HC, cut points (see details) 
}

\author{
Benoît Lalloué
}

\seealso{
\code{\link{SesClassif}}, \code{\link{plot.PCA}}, \code{\link{plot.HCPC}}
}
\examples{
SesData <- read.table(system.file("extdata","SesData.txt", package = "SesIndexCreatoR"),
                      header=TRUE,sep="\t", row.names=1)
varnames <- colnames(SesData)[2:ncol(SesData)]
group1 <- grep("+Unemployed", colnames(SesData), value=TRUE)
group2 <- grep("+LabourForce", colnames(SesData), value=TRUE)
groupvarnames <- list(group1, group2)
illus <- rownames(SesData[SesData[,"Type"] \%in\% c("A", "D"),])
index <- SesIndex(SesData, varnames=varnames, groupvarnames=groupvarnames, sup=illus)
classif1 <- SesClassif(index)
classif2 <- SesClassif(index, method="quantiles")

# Plot with default option
plot(classif1)
plot(classif2)

# Plot passing arguments from other function
plot(classif1, axes=c(3,4))
plot(classif2, label="none")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
