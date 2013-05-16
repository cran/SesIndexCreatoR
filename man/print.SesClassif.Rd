\name{print.SesClassif}
\alias{print.SesClassif}

\title{
Print the classification of a socioeconomic index results
}
\description{
Print the classification of a socioeconomic index results performed with \code{\link{SesClassif}} in order to make easier their reading.
}
\usage{
\method{print}{SesClassif}(x, ...)
}

\arguments{
  \item{x}{
An object resulting from the \code{\link{SesClassif}} function.
}
\item{\dots}{
 further arguments passed to or from other methods.
}
}

\author{
Benoît Lalloué
}

\seealso{
\code{\link{SesClassif}}
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

classif1
# or
print(classif1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
