SesStep1 <- function(X, varnames, groupvarnames, sup){
  # X is a dataframe containing the data
  # varnames is a character vector or a list of character vectors containing the names of the variables that must be included in the analysis
  # groupvarnames is a list containing the vectors of names of the redundant groups (one vector for each group) (default: NULL)
  # sup is a vector containing the names of the supplementary units (default: NULL)
 
  stopifnot(is.data.frame(X))
  stopifnot(is.null(varnames) || is.vector(varnames, mode="character") || is.list(varnames))
  stopifnot(all(unlist(varnames) %in% colnames(X)))
  stopifnot(is.null(groupvarnames) || (is.list(groupvarnames) && all(unlist(groupvarnames) %in% unlist(varnames))))
  stopifnot(is.null(sup) || any(sup %in% rownames(X)))
  
  # Extraction of the redundant groups and selection of the variable
  l <- length(groupvarnames)
  selection <- vector()
  results <- list()  
  for (i in 1:l){
    assign(paste("group",i, sep=""), groupvarnames[[i]])
    
    #PCA on the variables of the group, lines with missing values ommitted
    assign(paste("SelectVar.group",i, sep=""), SelectVar(X, sup=sup, varnames=get(paste("group",i, sep=""))))
    
    # Selection of the variable with the higher cos?
    selection[i] <- get(paste("SelectVar.group",i, sep=""))$selection
      
    results$analysis[[paste("group",i, sep="")]] <- get(paste("SelectVar.group",i, sep=""))$analysis
  }
  results$selection <- selection
  
  return(results)
}
