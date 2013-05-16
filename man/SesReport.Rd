\name{SesReport}
\alias{SesReport}

\title{
Creation of a report for SesIndex and SesClassif functions
}
\description{
Create a .html file with a report summarizing the results of the different steps of the creation of a socioeconomic index with the \code{\link{SesIndex}} function and, if any, for the classification of the index using the \code{\link{SesClassif}} function.
Create also a .csv file containing the original dataset and the index (crude and standardized) and, if any, the classification.
}
\usage{
SesReport(x, outdir=getwd(), name = "SesReport", export = TRUE, report = TRUE)
}
\arguments{
  \item{x}{
An object resulting from the \code{\link{SesIndex}} function or from the \code{\link{SesClassif}} function.
}
  \item{outdir}{
A character string. The name of the directory where the files will be created (by default, the current working directory).
}
  \item{name}{
A character string. The basename of the files to create, without extension.

}
  \item{export}{
A logical. Create a .csv file containing the original dataset and the index (crude and standardized) and, if any, the classification, or not.
}
  \item{report}{
A logical. Create a .html file containing the report or not.
}
}
\details{
This function create files containing different results from the \code{\link{SesIndex}} function and the \code{\link{SesClassif}} function.

By default, these files will be created in the current working directory. Otherwise, the files will be created in the \code{outdir} directory (which will be created if it does not exist). 
The basename \code{name} will be used as a prefix for all files. \emph{If a file already exists in the \code{outdir} directory, it will be overwritten}. Depending on the selected options and the \code{x} object, the following filenames can be used: \code{name}.csv, \code{name}.html, \code{name}-step1-0k.png (where k varies between 1 and the number of redundant groups), \code{name}-step2-ind.png, \code{name}-step2-var.png, \code{name}-step3-ind.png, \code{name}-step3-var.png, \code{name}-classif-dendro.png, \code{name}-classif-ind.png.
}
\value{
Files (.csv, .html, .png)
}

\author{
Benoît Lalloué
}

\seealso{
\code{\link{SesIndex}}, \code{\link{SesClassif}}
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
classif2 <- SesClassif(index, nb=5, method="quantiles")

\dontrun{
# Run the following lines will create files and directory in the current working directory
SesReport(index, name="index-alone")
SesReport(classif1, name="index-HC")
SesReport(classif2, outdir="SesReport", name="index-quantiles")
}
}

