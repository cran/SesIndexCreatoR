\name{print.SesIndex}
\alias{print.SesIndex}

\title{
Print the creation of a socioeconomic index results
}
\description{
Print the classification of a socioeconomic index results performed with \code{\link{SesClassif}} in order to make easier their reading.
}
\usage{
\method{print}{SesIndex}(x, ...)
}
\arguments{
  \item{x}{
An object resulting from the \code{\link{SesIndex}} function.
}
 \item{...}{
 further arguments passed to or from other methods.
}
}

\author{
Benoît Lalloué
}

\seealso{
\code{\link{SesIndex}}
}
\examples{
SesData <- read.table(system.file("extdata","SesData.txt", package = "SesIndexCreatoR"),
                      header=TRUE,sep="\t", row.names=1)
index1 <- SesIndex(SesData)

index1
# or
print(index1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
