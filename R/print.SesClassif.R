print.SesClassif = function (x, ...) {
  if (!inherits(x, "SesClassif")) 
    stop("Non convenient data")
  cat("**Classification of a socio-economic index**\n")
  cat("\t", length(table(x$table[,"Classes"])), "classes was created using:", x$call$method ,"\n\n")
  cat("*The results are available in the following objects:\n")
  res <- array("", c(3, 2), list(1:3, c("Name", "Description")))
  res[1, ] <- c("$analysis", switch(x$call$method, HC="Detailed results of the HC", 
                                    "HC+knn"="Detailed results of the HC", 
                                    quantiles="Cut points", 
                                    interval="Cut points"))
  res[2, ] <- c("$table", "Results of the classification")
  res[3, ] <- c("$call", "Arguments passed in the call of the function")
  print(res)
  
  cat("\n*Number of units in each class:")
  res2 <- table(x$table[,"Classes"])
  print(res2, ...)
}
