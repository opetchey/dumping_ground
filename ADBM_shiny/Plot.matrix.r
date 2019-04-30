
Plot.matrix <- function(web, title=" ", point.cex=0.5, trait.cex=1,
                        diag.line=T, traits=F, by.consumer=T, axes.labels=F, sp.pt.ch=NA){
    
  S <- length(web[,1])

  ##point.cex <- 30/30
  ##trait.cex <- 30/30
  
  dimnames(web) <- list(1:S, 1:S)
  consumer <- rep(1:S, each=S)
  resource <- rep(1:S, S)
  web.list <- Matrix.to.list(web)
  par(xpd=T, fin=c(4.5,5))
  plot(consumer, resource, pch=19, type="n", cex=0.1,
       ann=F, axes=F,
       xlim=c(1, S), ylim=c(1, S))
  if(length(traits)==1)
    points(web.list[,2], S+1-as.numeric(web.list[,1]),
           type="p", pch=19, cex=point.cex)
  if(length(traits)==length(web)){

    colours.to.use <- rev(heat.colors(30)[c(-1:-5, -26:-30)])
    ##colours.to.use <- rev(gray(1:30/30)[c(-1:-5, -26:-30)])
    
    if(by.consumer){
      integer.traits <- matrix(0, S, S)
      for(i in 1:S){
        traits.01 <- traits[,i]-min(traits[,i])
        traits.01 <- traits.01/max(traits.01)
        integer.traits[,i] <- round(traits.01*19)+1
        integer.traits[traits[,i]==0,i] = NaN
        
      }
    }
    
    if(!by.consumer){
      colours.to.use <- heat.colors(20)
      traits.01 <- traits-min(traits)
      traits.01 <- traits.01/max(traits.01)
      integer.traits <- round(traits.01*19)+1
    }

    if(point.cex>trait.cex){
        points(web.list[,2], S+1-as.numeric(web.list[,1]),
               type="p", pch=19, cex=point.cex, col="black")##colours.to.use[integer.traits])
        points(rep(1:S, each=S), rep(S:1, times=S), 
               pch=19, cex=trait.cex, col=colours.to.use[integer.traits])
    }
    if(point.cex<trait.cex){
        points(rep(1:S, each=S), rep(S:1, times=S), 
               pch=19, cex=trait.cex, col=colours.to.use[integer.traits])
        points(web.list[,2], S+1-as.numeric(web.list[,1]),
               type="p", pch=19, cex=point.cex, col="black")##colours.to.use[integer.traits])
    }

    if(!is.na(sp.pt.ch))
        points(web.list[,2], S+1-as.numeric(web.list[,1]),
               type="p", pch=sp.pt.ch, cex=point.cex, col="black")##colours.to.use[integer.traits])
        
    
  }
  par(xpd=F)
  mtext(side=3, text=title, font=2, line=2, cex=0.5)
  if(axes.labels){
      mtext(side=2, "Resource", line=0.5, cex=1)
      mtext(side=3, "Consumer", line=0.5, cex=1)
  }
  if(diag.line==T)
    lines(1:S, S:1, lty="dashed")

}



## takes a food web in matrix format and coverts it to list format
Matrix.to.list <- function(web.matrix, predator.first=TRUE){
  if(length(dimnames(web.matrix)[[1]])==length(web.matrix[1,]))
    species.names <- dimnames(web.matrix)[[1]]
  else
    species.names <- 1:length(web.matrix[,1])
  web.list <- matrix(0, sum(web.matrix), 2)
  counter <- 1
  for(i in 1:length(web.matrix[,1]))
    for(j in 1:length(web.matrix[,1]))
      if(web.matrix[i,j]==1){
        web.list[counter,] <- c(species.names[i],species.names[j])
        counter <- counter + 1
      }
  if(!predator.first)
    web.list <- cbind(web.list[,2], web.list[,1])
  web.list
}

