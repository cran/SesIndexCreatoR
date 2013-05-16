print.SesIndex = function (x, ...) {
  if (!inherits(x, "SesIndex")) {
    stop("Non convenient data")
  }
  if (inherits(x$step2$analysis, "MFA")) {
    step2.method <- "MFA"
  }
  else {
    step2.method <- "PCA"
  }
  cat("**Procedure of creation of a socio-economic index**\n")
  cat("*The results are available in the following objects:\n\n")
  res <- array("", c(10, 2), list(1:10, c("Name", "Description")))
  indice <- 1
  if (!is.null(x$step1)){
  res[indice, ] <- c("$step1", "Results of the first step")
  res[indice+1, ] <- c("$step1$analysis", "Detailed results of the PCA on each redundant group")
  res[indice+2, ] <- c("$step1$selection", "Names of the variable selected in each group")
  indice <- indice +3
  }
  if (!is.null(x$step2)){
  res[indice, ] <- c("$step2", "Results of the second step")
  res[indice+1, ] <- c("$step2$analysis", switch(step2.method, PCA="Detailed results of the selection step (PCA)",
                                                 MFA="Detailed results of the selection step (MFA)"))
  res[indice+2, ] <- c("$step2$selection", "Names of the selected variables")
  indice <- indice +3
  }
  if (!is.null(x$step3)){
  res[indice, ] <- c("$step3", "Results of the third step")
  res[indice+1, ] <- c("$step3$analysis", "Detailed results of the final PCA")
  res[indice+2, ] <- c("$step3$indices", "Original dataset and the computed socio-economic index")
  indice <- indice +3
  }
  res[indice, ] <- c("$call", "Arguments passed in the call of the function")
  print(res[1:(indice), ], ...)
}
