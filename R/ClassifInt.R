ClassifInt = function(x, nb){
  # x: a numeric vector
  # nb: number of classes
  stopifnot(is.vector(x) && is.numeric(x))
  stopifnot(nb > 0)
  
  tmp_min <- min(x, na.rm=T)
  tmp_max <- max(x, na.rm=T)
  bounds <- seq(tmp_min, tmp_max, by=(tmp_max-tmp_min)/nb)
  bounds[1] <- tmp_min-0.0001
  classes <- cut(x, bounds, labels=F)
  names(classes) <- names(x)
  
  results <- list("classes"=classes, "bounds"=bounds)
    
  return(results)
}

