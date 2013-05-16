ClassifHC <- function(X, nb, knn=FALSE){
  # X: object resulting from a PCA function
  # nb: number of classes
  # knn: use knn after HC or not
  stopifnot(nb==-1 || nb > 0)
  stopifnot(is.logical(knn))
  
  HC_results <- HCPC(X, nb.clust=nb, graph=F)
  
  ind_act <- X$ind$coord[order(row.names(X$ind$coord)),]
  class <- HC_results$data.clust[order(row.names(HC_results$data.clust)),"clust"]
  ind_illus <- X$ind.sup$coord
  
  ind_act_class <- cbind(ind_act, class)
  ind_tot <- ind_act_class  

  if (knn) {
    if (is.null(X$ind.sup)) {
      warning("No supplementary units defined: k-nearest neighbour classification was not applied.", call.=FALSE)
    }
    else {
      tmp_classement <- knn(ind_act,ind_illus,class,k=10,prob=FALSE)
      classement <- as.numeric(tmp_classement)
      names(classement) <- names(tmp_classement)
      
      ind_illus_class <- cbind(ind_illus, classement)
      colnames(ind_illus_class) <- colnames(ind_act_class)
      ind_tot <- rbind(ind_act_class,ind_illus_class)
    }
  }    
  classes <- ind_tot[,ncol(ind_tot)]
  names(classes) <- rownames(ind_tot)
  
  results <- list("classes"=classes, "analysis"=HC_results)

  return(results)
}
