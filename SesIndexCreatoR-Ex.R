pkgname <- "SesIndexCreatoR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('SesIndexCreatoR')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("SesClassif")
### * SesClassif

flush(stderr()); flush(stdout())

### Name: SesClassif
### Title: Create categories from a socioeconomic index
### Aliases: SesClassif
### Keywords: cluster

### ** Examples

# Importation of the table (column name are on the first row, units name are on the first column, column are separated by a tabulation)
SesData <- read.table(system.file("extdata","SesData.txt", package = "SesIndexCreatoR"), header=TRUE,sep="\t", row.names=1)

# Creation of the socioeconomic index with SesIndex
varnames <- colnames(SesData)[2:ncol(SesData)]
group1 <- grep("+Unemployed", colnames(SesData), value=TRUE)
group2 <- grep("+LabourForce", colnames(SesData), value=TRUE)
groupvarnames <- list(group1, group2)
illus <- rownames(SesData[SesData[,"Type"] %in% c("A", "D"),])
index <- SesIndex(SesData, varnames=varnames, groupvarnames=groupvarnames, sup=illus)

# Classification with default options (automatic number of classes, Hierarchical Clustering and k nearest neighbors)
classif1 <- SesClassif(index)
plot(classif1)

# Classification in 5 classes with quantiles
classif2 <- SesClassif(index, nb=5, method="quantiles")
plot(classif2)



cleanEx()
nameEx("SesIndex")
### * SesIndex

flush(stderr()); flush(stdout())

### Name: SesIndex
### Title: Creation of a Socio-Economic Index
### Aliases: SesIndex
### Keywords: multivariate

### ** Examples

# Importation of the table (column name are on the first row, units name are on the first column, column are separated by a tabulation)
SesData <- read.table(system.file("extdata","SesData.txt", package = "SesIndexCreatoR"), header=TRUE,sep="\t", row.names=1)

# Extraction of the names of the variables to analyse as a vector
varnames <- colnames(SesData)[2:ncol(SesData)]
# Extraction of the names of the variables to analyse as a list: 5 categories of variables are considered (demography, employment, education, housing, immigration)
varnames_MFA <- list("demography"=varnames[c(1,2,17)], "employement"=varnames[c(4:16,37)], "education"=varnames[c(18,20:25)], "housing"=varnames[c(19,26:36)], "immigration"=varnames[3])

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
illus <- rownames(SesData[SesData[,"Type"] %in% c("A", "D"),])

# SesIndex with supplementary units and all the steps
index5 <- SesIndex(SesData, varnames=varnames, groupvarnames=groupvarnames, sup=illus)
plot(index5)




cleanEx()
nameEx("SesReport")
### * SesReport

flush(stderr()); flush(stdout())

### Name: SesReport
### Title: Creation of a report for SesIndex and SesClassif functions
### Aliases: SesReport

### ** Examples

SesData <- read.table(system.file("extdata","SesData.txt", package = "SesIndexCreatoR"), header=TRUE,sep="\t", row.names=1)
varnames <- colnames(SesData)[2:ncol(SesData)]
group1 <- grep("+Unemployed", colnames(SesData), value=TRUE)
group2 <- grep("+LabourForce", colnames(SesData), value=TRUE)
groupvarnames <- list(group1, group2)
illus <- rownames(SesData[SesData[,"Type"] %in% c("A", "D"),])
index <- SesIndex(SesData, varnames=varnames, groupvarnames=groupvarnames, sup=illus)
classif1 <- SesClassif(index)
classif2 <- SesClassif(index, nb=5, method="quantiles")

## Not run: 
##D # Run the following lines will create files and directory in the current working directory
##D SesReport(index, name="index-alone")
##D SesReport(classif1, name="index-HC")
##D SesReport(classif2, outdir="SesReport", name="index-quantiles")
## End(Not run)



cleanEx()
nameEx("plot.SesClassif")
### * plot.SesClassif

flush(stderr()); flush(stdout())

### Name: plot.SesClassif
### Title: Plot the results of the classification of a socioeconomic index
### Aliases: plot.SesClassif

### ** Examples

SesData <- read.table(system.file("extdata","SesData.txt", package = "SesIndexCreatoR"), header=TRUE,sep="\t", row.names=1)
varnames <- colnames(SesData)[2:ncol(SesData)]
group1 <- grep("+Unemployed", colnames(SesData), value=TRUE)
group2 <- grep("+LabourForce", colnames(SesData), value=TRUE)
groupvarnames <- list(group1, group2)
illus <- rownames(SesData[SesData[,"Type"] %in% c("A", "D"),])
index <- SesIndex(SesData, varnames=varnames, groupvarnames=groupvarnames, sup=illus)
classif1 <- SesClassif(index)
classif2 <- SesClassif(index, method="quantiles")

# Plot with default option
plot(classif1)
plot(classif2)

# Plot passing arguments from other function
plot(classif1, axes=c(3,4))
plot(classif2, label=FALSE)



cleanEx()
nameEx("plot.SesIndex")
### * plot.SesIndex

flush(stderr()); flush(stdout())

### Name: plot.SesIndex
### Title: Plot the results of the construction of a socioeconomic index
### Aliases: plot.SesIndex

### ** Examples

SesData <- read.table(system.file("extdata","SesData.txt", package = "SesIndexCreatoR"), header=TRUE,sep="\t", row.names=1)
varnames <- colnames(SesData)[2:ncol(SesData)]
group1 <- grep("+Unemployed", colnames(SesData), value=TRUE)
group2 <- grep("+LabourForce", colnames(SesData), value=TRUE)
groupvarnames <- list(group1, group2)
illus <- rownames(SesData[SesData[,"Type"] %in% c("A", "D"),])

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
plot(index, label=FALSE)



cleanEx()
nameEx("print.SesClassif")
### * print.SesClassif

flush(stderr()); flush(stdout())

### Name: print.SesClassif
### Title: Print the classification of a socioeconomic index results
### Aliases: print.SesClassif

### ** Examples

SesData <- read.table(system.file("extdata","SesData.txt", package = "SesIndexCreatoR"), header=TRUE,sep="\t", row.names=1)
varnames <- colnames(SesData)[2:ncol(SesData)]
group1 <- grep("+Unemployed", colnames(SesData), value=TRUE)
group2 <- grep("+LabourForce", colnames(SesData), value=TRUE)
groupvarnames <- list(group1, group2)
illus <- rownames(SesData[SesData[,"Type"] %in% c("A", "D"),])
index <- SesIndex(SesData, varnames=varnames, groupvarnames=groupvarnames, sup=illus)

classif1 <- SesClassif(index)

classif1
# or
print(classif1)




cleanEx()
nameEx("print.SesIndex")
### * print.SesIndex

flush(stderr()); flush(stdout())

### Name: print.SesIndex
### Title: Print the creation of a socioeconomic index results
### Aliases: print.SesIndex

### ** Examples

SesData <- read.table(system.file("extdata","SesData.txt", package = "SesIndexCreatoR"), header=TRUE,sep="\t", row.names=1)
index1 <- SesIndex(SesData)

index1
# or
print(index1)




### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
