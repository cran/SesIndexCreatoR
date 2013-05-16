SesClassif <- function(X, nb="auto", method="HC+knn"){
  # X is an object resulting of the SesIndex function
  # nb: "auto" or integer
  # method: "HC", "HC+knn", "quantiles", "interval"

  # Arguments checking
  stopifnot(inherits(X, "SesIndex"))
  stopifnot(nb=="auto" ||  nb>0)
  stopifnot(method %in% c("HC", "HC+knn", "quantiles", "interval"))
  
  # Preliminary steps
  X.analysis <- X$step3$analysis
  X.data <- X$step3$indices
  tmp_ind <- X.data[,"Standardized SES Index"]
  names(tmp_ind) <- rownames(X.data)
  
  # Saving the call arguments
  results <- list()
  results$call <- list("X"=X, "nb"=nb, "method"=method)
  
  # Definition of the "auto" number of variables
  if (nb=="auto"){
    nb <- switch(method, HC=-1, "HC+knn"=-1, quantiles=4, interval=4)
  }
  
  tmp_classif <- switch(method, HC=ClassifHC(X.analysis, nb), 
                        "HC+knn"=ClassifHC(X.analysis, nb, knn=TRUE), 
                        "quantiles"=ClassifQuant(tmp_ind, nb), 
                        "interval"=ClassifInt(tmp_ind, nb))
  classes <- switch(method, HC=tmp_classif$classes, 
                    "HC+knn"=tmp_classif$classes, 
                    "quantiles"=tmp_classif$classes, 
                    "interval"=tmp_classif$classes)
  results$analysis <- switch(method, HC=tmp_classif$analysis, 
                             "HC+knn"=tmp_classif$analysis, 
                             "quantiles"=tmp_classif$bounds, 
                             "interval"=tmp_classif$bounds)
  
  X_merge <- merge(X.data, classes, all.x=TRUE, sort=FALSE, by=0)
  # Adding more explicit names
  rownames(X_merge) <- X_merge[,"Row.names"]
  names(X_merge)[ncol(X_merge)] <- "Classes"
  # Sort by census block inditification
  X_merge <- X_merge[order(rownames(X_merge)),2:ncol(X_merge)]

  results$table <- X_merge
  class(results) <- c("SesClassif", "list")
  return(results)
}
          
