SesIndex <- function(X, varnames=NULL, groupvarnames=NULL, sup=NULL, method="PCA", step="all"){
  # X is a dataframe containing the data
  # varnames is a character vector or a list of character vectors containing the names of the variables that must be included in the analysis
  # groupvarnames is a list containing the vectors of names of the redundant groups (one vector for each group) (default: NULL)
  # sup is a vector containing the names of the supplementary units (default: NULL)
  # method : "PCA" or "MFA"
  # step : "1" (reduction of redundant groups in groupvarnames), "2" (selection of the variables), "3" (construction of the index and export) or c("1","2") or c("2","3") or "all" (default:"all")
  
  # Arguments checking
  stopifnot(is.data.frame(X))
  stopifnot(is.null(varnames) || is.vector(varnames, mode="character") || is.list(varnames))
  stopifnot(all(unlist(varnames) %in% colnames(X)))
  stopifnot(is.null(groupvarnames) || (is.list(groupvarnames) && all(unlist(groupvarnames) 
                                                                     %in% unlist(varnames))))
  stopifnot(is.null(sup) || any(sup %in% rownames(X)))
  stopifnot(step %in% c("1", "2", "3", "all"))
  stopifnot( method %in% c("PCA", "MFA"))

  # Saving the call arguments
  results <- list()  
  results$call <- list("X"=X, "varnames"=varnames, "groupvarnames"=groupvarnames, 
                       "sup"=sup, "method"=method, "step"=step)
  
  # Use all numerical variables if no varnames specified
  if (is.null(varnames)){
    varnames <- colnames(X[,sapply(X, is.numeric)])
  }
  
  # Step 1 
  if (1 %in% step || step=="all") {
    if (is.null(groupvarnames)) {
      selection <- NULL
      warning("No groupvarnames defined: step 1 skipped.", call.=FALSE)
    }
    else {
      tmp_step1 <- SesStep1(X, varnames=varnames,
                            groupvarnames=groupvarnames, sup=sup)
      results$step1$analysis <- tmp_step1$analysis
      selection <- tmp_step1$selection
    }
    results$step1$selection <- selection
  }
  
  #Step 2
  if (2 %in% step || step=="all") {
    if (!exists("selection")) {
      selection <- unlist(groupvarnames)
      warning("Step 1 has been skipped: step 2 executed on all the variables in varnames", call.=FALSE)
    }
    # Definition of the list of variables after reduction of redundant groups
    tmp_reduc <- unlist(lapply(groupvarnames, setdiff, selection))
    if (is.list(varnames)){      
      var_reduc <- lapply(varnames, setdiff, tmp_reduc)
    }
    else {
      var_reduc <- setdiff(varnames, tmp_reduc)
    }
    # Selection of the variables
    SelectVar.reduc <- SelectVar(X, sup=sup, varnames=var_reduc, 
                                 nb="mean", method=method)
    var_select <- SelectVar.reduc$selection
    
    results$step2 <- SelectVar.reduc    
  }
  
  # Step 3
  if (3 %in% step || step=="all") {
    if (!exists("var_select")) {
      var_select <- varnames
      warning("Step 2 has been skipped: step 3 executed on all the variables in varnames", call.=FALSE)
    }
    # Final PCA
    SelectVar.final <- SelectVar(X, sup=sup, 
                                 varnames=var_select, nb="mean")
    
    results$step3$analysis <- SelectVar.final$analysis
   
    # Merging of the results for active individuals and supplementary individuals
    tmp_index <- rbind(SelectVar.final$analysis$ind$coord, 
                       SelectVar.final$analysis$ind.sup$coord)
    index <- tmp_index[,1]
    names(index) <- rownames(tmp_index)
    
    # Computation of the standardized index
    index_std <- index/sd(index)
    # Merging of the index and standardized index
    indices <- cbind(index, index_std)
    
    # Merging of the original dataset with the two indexes
    X_merge <- merge(X, indices, all.x=TRUE, sort=FALSE, by=0)
    rownames(X_merge) <- X_merge[,"Row.names"]
    names(X_merge)[c(ncol(X_merge)-1,ncol(X_merge))] <- c("SES Index", 
                                                          "Standardized SES Index")
    X_merge <- X_merge[order(rownames(X_merge)),2:ncol(X_merge)]

    results$step3$indices <- X_merge
  }
  class(results) <- c("SesIndex", "list")
  return(results)
}