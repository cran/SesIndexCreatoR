plot.SesIndex <- function(x, step="all", choice="mixed", ...){
  if (!inherits(x, "SesIndex")) 
    stop("Non convenient data")  
  stopifnot(step %in% c("1", "2", "3", "all"))  
  stopifnot(choice %in% c("var","ind", "mixed"))
  
  def.par <- par(no.readonly = TRUE)  
  #plot.new()
  choix <- choice
  l <- length(x$step1$analysis)
  
  if (step=="1" && !is.null(x$step1)){
   for (i in 1:l){
    tmp_grp <- paste("group",i, sep="")
    tmp_plot <- x$step1$analysis[[tmp_grp]]
    if (choix=="mixed"){
     nf <- layout(matrix(c(1, 2), 1,2, byrow=T), T)
     plot(tmp_plot, choix="var", title="Correlations circle", ...)
     plot(tmp_plot, choix="ind", title="Projections of the units", ...)
     par(oma=c(0,0,1,0))
     title(paste("Summary of the Step 1: Group", i), outer=T)
     par(def.par)
    }
    else {
     plot(tmp_plot, choix=choix, title=paste("Step 1 analysis: Group", i), ...)
    }
   }
  }
  if (step=="2" && !is.null(x$step2)){
    if (choix=="mixed"){
      nf <- layout(matrix(c(1, 2), 1,2, byrow=T), T)  
      #layout.show(nf)  
      plot(x$step2$analysis, choix="var", title="Correlations circle", ...)  
      plot(x$step2$analysis, choix="ind", title="Projections of the units", ...)
      par(oma=c(0,0,1,0))
      title("Summary of the Step 2", outer=T)
      par(def.par)
    }
    else {
      plot(x$step2$analysis, choix=choix, title="Step 2 analysis", ...)
    }
  }
  if (step=="3" && !is.null(x$step3)){
    if (choix=="mixed"){
      nf <- layout(matrix(c(1, 2), 1,2, byrow=T), T)  
      #layout.show(nf)  
      plot(x$step3$analysis, choix="var", title="Correlations circle", ...)  
      plot(x$step3$analysis, choix="ind", title="Projections of the units", ...)
      par(oma=c(0,0,1,0))
      title("Summary of the Step 3", outer=T)
      par(def.par)
    }
    else {
      plot(x$step3$analysis, choix=choix, title="Step 3 analysis", ...)
    }    
  }
  if (step=="all"){
    if (!is.null(x$step1)){
      for (i in 1:l){
        tmp_grp <- paste("group",i, sep="")
        if (choix=="var" || choix=="mixed"){
          tmp_disp <- round(cbind("coord"=x$step1$analysis[[tmp_grp]]$var$coord[,1], "cos2"=x$step1$analysis[[tmp_grp]]$var$cos2[,1], "contrib"=x$step1$analysis[[tmp_grp]]$var$contrib[,1]), 2)
          cat("GROUP",i ,"\n")
          cat("Summary of the variables' correlations, cos2 and contributions for the first component","\n")
          print(tmp_disp)
          cat("\n")
        }
        if (choix=="ind" || choix=="mixed"){
          ##tmp_min <- min(5,ncol(tmp$step1$analysis$group2$ind$coord))
          tmp_min <- min(5,ncol(x$step1$analysis$group2$ind$coord))
          tmp_disp <- summary(x$step1$analysis[[tmp_grp]]$ind$coord[,1:tmp_min])
          cat("GROUP",i, "\n")
          cat("Summary of the coordinates of the individuals on the components","\n")
          print(tmp_disp)
          cat("\n")
        }        
      }
    }    
    if (choix=="mixed"){
      nf <- layout(matrix(c(1, 2), 1,2, byrow=T), c(3.5,4.5), T)  
      #layout.show(nf)  
      plot(x$step2$analysis, choix="var", title="Step 2 analysis", ...)  
      plot(x$step3$analysis, choix="var", title="Step 3 analysis", ...)
      par(oma=c(0,0,1,0))
      title("Summary of the creation procedure - Variables", outer=T)
      par(def.par)
      dev.new()
      nf <- layout(matrix(c(1, 2), 1,2, byrow=T), c(3.5,4.5), T)      
      plot(x$step2$analysis, choix="ind", title="Step 2 analysis", ...)  
      plot(x$step3$analysis, choix="ind", title="Step 3 analysis", ...)
      par(oma=c(0,0,1,0))
      title("Summary of the creation procedure - Units", outer=T)
      par(def.par)
    }
    else {
      nf <- layout(matrix(c(1, 2), 1,2, byrow=T), c(3.5,4.5), T)  
      #layout.show(nf)  
      plot(x$step2$analysis, choix=choix, title="Step 2 analysis", ...)  
      plot(x$step3$analysis, choix=choix, title="Step 3 analysis", ...)
      par(oma=c(0,0,1,0))
      title("Summary of the creation procedure", outer=T)
      par(def.par)
    }
  }    
  par(def.par)  
}
