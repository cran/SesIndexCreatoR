SelectVar <- function(X, sup=NULL, varnames=NULL, nb=1, method="PCA"){
  # X : dataframe containing the data (with ID of the rows and names of the variables)
  # sup : character vector containing the rownames of the supplementary units (default : NULL)
  # varnames : character vector or list of character vectors. Names of the variables among which the selection must occur. For PCA, varname can be a vector. For MFA, varnames must be a list containing a vector of variable names for each group of variables and the names of the different groups (see examples) (default : all the column of the dataframe)
  # nb : integer or "mean". Number of variables to select (if integer) or select all variables with a correlation with the first axe greater than the mean correlation (default=1)
  # method : "PCA" or "MFA". Type of factorial analysis used, Principal Components Analysis or Multiple Factor Analysis (default: "PCA")
  
  # Verification that all arguments meet the conditions and of the presence of required libraries
  stopifnot(is.data.frame(X))
  stopifnot(is.null(varnames) || is.vector(varnames, mode="character") || is.list(varnames))
  stopifnot(all(unlist(varnames) %in% colnames(X)))
  stopifnot(is.null(sup) || any(sup %in% rownames(X)))
  stopifnot(nb>0 || nb=='mean')
  stopifnot(method %in% c("PCA","MFA"))

  if (is.null(varnames)){
    varnames <- colnames(X[,sapply(X, is.numeric)])
  }
            
  # Preparation of the supplementary units indices 
  if (is.null(sup)) {
    index_sup <- NULL
  }
  else {
    index_sup <- na.omit(match(sup,rownames(na.omit(X[,unlist(varnames)]))))
  }
  
  if (nb!="mean" && nb>length(unlist(varnames))) {
    nb <- length(unlist(varnames))
    warning("nb superior to the number of selected variables: nb fixed to the number of selected variables.", call.=FALSE)
  }
  
  if (method=="PCA"){
    #PCA on the variables
    results <- PCA(na.omit(X[,unlist(varnames)]), ind.sup=index_sup, ncp=10, graph=FALSE)
    if (nb=="mean") {
      tmp_select <- results$var$contrib[,1] > 1/ncol(results$call$X)*100
      selection <- names(tmp_select)[tmp_select]
    }
    else {
      tmp_select <- results$var$cos2[,1]
      selection <- names(tmp_select[order(tmp_select, decreasing=T)])[1:nb]
    }
  }
  else {
    #MFA on the variables
    if(!is.list(varnames)){
      warning("varnames is not a list: MFA performed on a single group and results are equivalent to a PCA", call.=FALSE)
    }
    results <- MFA(na.omit(X[,unlist(varnames)]), group=sapply(varnames, length), 
                   name.group=names(varnames), ind.sup=index_sup, ncp=10, graph=FALSE)
    if (nb=="mean") {
      tmp_select <- results$quanti.var$contrib[,1] > 1/ncol(results$call$X)*100
      selection <- names(tmp_select)[tmp_select]
    }
    else {
      tmp_select <- results$quanti.var$cos2[,1]
      selection <- names(tmp_select[order(tmp_select, decreasing=T)])[1:nb]
    }
  }
  values <- list("analysis"=results,"selection"=selection)
  return(values)
}
