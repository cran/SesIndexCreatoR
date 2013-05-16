\name{plot.SesIndex}
\alias{plot.SesIndex}

\title{
Plot the results of the construction of a socioeconomic index
}
\description{
Plot the results of the construction of a socioeconomic index, with a summary of the different steps or step by step, and with the different options provided by \code{\link{plot.PCA}}

}
\usage{
\method{plot}{SesIndex}(x, step = "all", choice = "mixed", ...)
}

\arguments{
  \item{x}{
An object resulting from the \code{\link{SesIndex}} function.
}
  \item{step}{
Either 1, 2, 3 or \dQuote{all}. The step(s) of the procedure to display.
}
  \item{choice}{
\dQuote{ind}, \dQuote{var} or \dQuote{mixed}. The type of graph to plot (see details).
}
  \item{\dots}{
Further arguments passed to or from other methods (see details).
}
}
\details{
This function displays the graphic results of the procedure of creation of a socio-economic index with the \code{\link{SesIndex}} function.

If \code{step} is set to \dQuote{all}, the function displays a summary of the different steps of the procedure. A summary of the results of the groups in step 1 is printed and the graphics of step 2 and 3 are plotted.
Else, only the results of the specified step are displayed.

If \code{choice} is \dQuote{mixed} then the results displayed are about both variables (correlations circle) and individual (projection of the units).
Else, only variables or individuals results are displayed.

Arguments for other functions can be passed in \code{\dots}, especially arguments from \code{\link{plot.PCA}} or \code{\link{plot.MFA}} for the choice of axes, labels, colors, ...
}
\value{
Return the projections of the units or the variables correlations circles.
Print the results by group for step 1.
}

\author{
Benoît Lalloué
}

\seealso{
\code{\link{SesIndex}}, \code{\link{plot.PCA}}, \code{\link{plot.MFA}}
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

# Plot with default option (summary of the different steps)
plot(index)

# Plot variables only
plot(index, choice="var")

# Plot step 3 only
plot(index, step="3")

# Plot only units for step 2
plot(index, choice="ind", step="2")

# Plot results for principal components 3 and 4 (passing arguments to the function plot.PCA)
plot(index, axes=c(3,4))

# Plot results without labels (passing arguments to the function plot.PCA)
plot(index, label="none")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
