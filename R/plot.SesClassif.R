plot.SesClassif <- function(x, ...){
  if (!inherits(x, "SesClassif")) 
    stop("Non convenient data")  

  def.par <- par(no.readonly = TRUE)
  x.method <- x$call$method
  x.analysis <- x$analysis
  
  if (x.method %in% c("quantiles", "interval")){
    tmp_X <- x$call$X$step3$analysis$ind$coord
    tmp_X2 <- x$call$X$step3$analysis$ind.sup$coord
    tmp_bind <- rbind(tmp_X, tmp_X2)
    tmp_classes <- as.vector(x$table[,"Classes"])
    names(tmp_classes) <- rownames(x$table)
    X <- merge(tmp_bind, tmp_classes, all.x=TRUE, sort=FALSE, by=0)
    rownames(X) <- X[,"Row.names"]
    X <- X[order(rownames(X)),2:ncol(X)]
    Y = X[, -ncol(X)]
    leg.map = NULL
    for (p in 1:nrow(X)) {
      leg.map[p] = paste("cluster",X[p,"y"], " ", sep = " ")
    }
    Y = cbind.data.frame(Y, as.factor(leg.map))
    res2 = PCA(Y, quali.sup = ncol(Y), scale.unit = FALSE, ncp = Inf, graph = FALSE)
    plot.PCA(res2, habillage = ncol(Y), cex = 0.8, ...)
  
    cat("Cut points","\n")
    print(x.analysis)

  }
  if (x.method %in% c("HC", "HC+knn")){
    plot(x.analysis, choice="tree", ...) 
    par(def.par)
    plot(x.analysis, choice="map", draw.tree=F, ...)    
  }
  par(def.par)
}