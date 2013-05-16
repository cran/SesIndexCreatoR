ClassifQuant <- function(x, nb){
  # x: a numeric vector
  # nb: number of classes
  stopifnot(is.vector(x) && is.numeric(x))
  stopifnot(nb > 0)
  
  tmp_probs <- seq(0,1,by=1/nb)
  bounds <- quantile(x, probs=tmp_probs, na.rm=T)
  bounds[1] <- bounds[1]-0.0001
  classes <- cut(x, bounds, labels=F)
  names(classes) <- names(x)
  
  results <- list("classes"=classes, "bounds"=bounds)
  
  return(results)
}