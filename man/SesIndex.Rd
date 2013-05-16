\name{SesIndex}
\alias{SesIndex}
\title{
Creation of a Socio-Economic Index
}
\description{
Creates a Socio-Economic Index such as defined in the Equit'Area project (see reference) with selection of the variables included in the index. It is possible to choose the start set of variables, the potential redundant groups of variables, the potential supplementary units, the method of selection and the step of the procedure to perform. Results included the final index and all the results of the intermediate steps.
}
\usage{
SesIndex(X, varnames = NULL, groupvarnames = NULL, sup = NULL, method = "PCA", 
          step = "all")
}

\arguments{
  \item{X}{
A dataframe containing the data.
}
  \item{varnames}{
A vector or a list of vectors (see details). The names of the variables to include in the procedure (by default, all the numeric variables in X).
}
  \item{groupvarnames}{
A list of vectors. Each vector containing the names of the variables of a redundant group.
}
  \item{sup}{
A vector. The names of the supplementary units. 
}
  \item{method}{
A character string. By default \dQuote{PCA} and the selection step performs a Principal Component Analysis to select the variable included in the index, else \dQuote{MFA} uses a Multiple Factor Analysis.
}
  \item{step}{
Either 1, 2, 3, c(1,2), c(1,3), c(2,3) or \dQuote{all}. The step(s) of the procedure to perform.
}
}
\details{
This function creates a socio-economic index such as defined in the Equit'Area project (see ref). The procedure uses datamining technics for select and combine variables in an one-dimensional index, in three steps.

First step is reducing the redundant groups (which contains variables that represent the same notion, such as unemployment among the total population, unemployment among the men population, unemployment among the women population). For each group, a PCA is performed on these variables and only the most correlated with the first component is kept. The redundant groups are defined by the argument \code{groupvarnames} : each element of the list should be a vector representing a group, and each vector contains the names of the variables in the group.

Second step is selecting the variables which will be in the index, either using a PCA or a MFA, by keeping only the variables with a correlation with the first component higher than the average correlation. 
If a PCA is used, \code{varnames} can be either a vector or a list of variable names. If MFA is used, \code{varnames} should be a list where each element is a group of variables represented by a vector containing their names (if \code{varnames} is not a list, then the MFA will be perform on one single group and the the results will be identical to a PCA). In both cases, if step 1 was performed before, the list of variables is \code{varnames} less (\code{groupvarnames} less the variables selected in step 1). If the step 1 was not performed before, all the variables in \code{varnames} will be used.

Third step is creating the index by performing a PCA on the variables selected in step 2 and keeping the first component. If step 2 was not performed before, all the variables in \code{varnames} will be used.

In each step, if \code{sup} is not \code{NULL}, all units present both in \code{sup} and \code{X} are considered as supplementary units.

In all the function, if some values are missing in a variable included in a part of the analysis, all the corresponding rows are dropped (using \code{\link{na.omit}}) for this part of the analysis. 
}
\value{
Returns a list including :
\item{step1$analysis}{Detailed results of the PCA on each redundant group, such as results provided by \code{\link{PCA}}.}
\item{step1$selection}{Vector containing the names of the variable selected in each group.}
\item{step2$analysis}{Detailed results of the selection step, PCA or MFA, such as results provided by \code{\link{PCA}} or \code{\link{MFA}}.}
\item{step2$selection}{Vector containing the names of the selected variables.}
\item{step3$analysis}{Detailed results of the final PCA, such as results provided by \code{\link{PCA}}.}
\item{step3$indices}{Dataframe containing the original dataset and the computed socio-economic index (crude and standardized versions).}
\item{call}{Arguments passed in the call of the function.}

N.B.: if a step was not performed, the correspondant part of the result do not exist.
%% ...
}

\references{
Lalloué et al.(2013) A statistical procedure to create a neighborhood socioeconomic index for health inequalities analysis. \emph{International Journal for Equity in Health} \bold{12},21
}

\author{
Benoît Lalloué
}

\seealso{
\code{\link{PCA}}, \code{\link{MFA}}, \code{\link{print.SesIndex}}, \code{\link{plot.SesIndex}}, \code{\link{SesClassif}}, \code{\link{SesReport}}
}
\examples{
# Importation of the table: column name are on the first row, 
# units name are on the first column, 
# column are separated by a tabulation
SesData <- read.table(system.file("extdata","SesData.txt", package = "SesIndexCreatoR"),
                      header=TRUE,sep="\t", row.names=1)

# Extraction of the names of the variables to analyse as a vector
varnames <- colnames(SesData)[2:ncol(SesData)]
# Extraction of the names of the variables to analyse as a list: 5 categories of variables are 
# considered (demography, employment, education, housing, immigration)
varnames_MFA <- list("demography"=varnames[c(1,2,17)], "employement"=varnames[c(4:16,37)],
                      "education"=varnames[c(18,20:25)], "housing"=varnames[c(19,26:36)],
                      "immigration"=varnames[3])

# SesIndex created with default options
index1 <- SesIndex(SesData)

# SesIndex with only the third step
index2 <- SesIndex(SesData, step="3")

# SesIndex using MFA in the second step
index3 <- SesIndex(SesData, varnames=varnames_MFA, method="MFA")

# Extraction of the names of the variables in the redundant groups
group1 <- grep("+Unemployed", colnames(SesData), value=TRUE)
group2 <- grep("+LabourForce", colnames(SesData), value=TRUE)
groupvarnames <- list(group1, group2)

# SesIndex with the first step including redundant groups
index4 <- SesIndex(SesData, varnames=varnames, groupvarnames=groupvarnames)

# Extraction of the names of the supplementary units
illus <- rownames(SesData[SesData[,"Type"] \%in\% c("A", "D"),])

# SesIndex with supplementary units and all the steps
index5 <- SesIndex(SesData, varnames=varnames, groupvarnames=groupvarnames, sup=illus)
plot(index5)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
