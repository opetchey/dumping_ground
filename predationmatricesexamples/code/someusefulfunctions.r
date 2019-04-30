
Plot.matrix <- function(web, title=" ",
						point.cex=0.5,
						trait.cex=1,
                        diag.line=T,
                        traits=F,
                        by.consumer=T,
                        axes.labels=F,
                        sp.pt.ch=NA,
                        pt.col="black",
                        grid=T,
                        box=T){
    
  S <- length(web[,1])

  dimnames(web) <- list(1:S, 1:S)
  consumer <- rep(1:S, each=S)
  resource <- rep(1:S, S)
  web.list <- Matrix.to.list(web)
  plot(consumer, resource, pch=19, type="n", cex=0.1,
       ann=F, axes=F,
       xlim=c(1, S), ylim=c(1, S))
  if(grid) {
    abline(h=1:S, col="lightgrey", lty="dotted")
    abline(v=1:S, col="lightgrey", lty="dotted")
  }
  if(box)
    box()

  par(xpd=T)

  if(length(traits)==1)
    points(web.list[,2], S+1-as.numeric(web.list[,1]),
           type="p", pch=19, cex=point.cex, col=pt.col)
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





# # ## some code to plot a predation matrix
# Plot.matrix <- function(web, title=" ", point.cex=0.5, trait.cex=1,
                        # diag.line=T, traits=F, by.consumer=T, axes.labels=F, sp.pt.ch=NA,
                        # grid=T, box=T){
    
  # S <- length(web[,1])

  # ##point.cex <- 30/30
  # ##trait.cex <- 30/30
  
  # dimnames(web) <- list(1:S, 1:S)
  # consumer <- rep(1:S, each=S)
  # resource <- rep(1:S, S)
  # web.list <- Matrix.to.list(web)

  # plot(consumer, resource, pch=19, type="n", cex=0.1,
       # ann=F, axes=F,
       # xlim=c(1, S), ylim=c(1, S))
  # if(grid) {
  	# abline(h=1:S, col="lightgrey", lty="dotted")
  	# abline(v=1:S, col="lightgrey", lty="dotted")
  # }
  # if(box)
    # box()
  # par(xpd=T)
  # if(length(traits)==1)
    # points(web.list[,2], S+1-as.numeric(web.list[,1]),
           # type="p", pch=19, cex=point.cex)
  # if(length(traits)==length(web)){

    # colours.to.use <- rev(heat.colors(30)[c(-1:-5, -26:-30)])
    # ##colours.to.use <- rev(gray(1:30/30)[c(-1:-5, -26:-30)])
    
    # if(by.consumer){
      # integer.traits <- matrix(0, S, S)
      # for(i in 1:S){
        # traits.01 <- traits[,i]-min(traits[,i])
        # traits.01 <- traits.01/max(traits.01)
        # integer.traits[,i] <- round(traits.01*19)+1
        # integer.traits[traits[,i]==0,i] = NaN
        
      # }
    # }
    
    # if(!by.consumer){
      # colours.to.use <- heat.colors(20)
      # traits.01 <- traits-min(traits)
      # traits.01 <- traits.01/max(traits.01)
      # integer.traits <- round(traits.01*19)+1
    # }

    # if(point.cex>trait.cex){
        # points(web.list[,2], S+1-as.numeric(web.list[,1]),
               # type="p", pch=19, cex=point.cex, col="black")##colours.to.use[integer.traits])
        # points(rep(1:S, each=S), rep(S:1, times=S), 
               # pch=19, cex=trait.cex, col=colours.to.use[integer.traits])
    # }
    # if(point.cex<trait.cex){
        # points(rep(1:S, each=S), rep(S:1, times=S), 
               # pch=19, cex=trait.cex, col=colours.to.use[integer.traits])
        # points(web.list[,2], S+1-as.numeric(web.list[,1]),
               # type="p", pch=19, cex=point.cex, col="black")##colours.to.use[integer.traits])
    # }

    # if(!is.na(sp.pt.ch))
        # points(web.list[,2], S+1-as.numeric(web.list[,1]),
               # type="p", pch=sp.pt.ch, cex=point.cex, col="black")##colours.to.use[integer.traits])
        
    
  # }
  # par(xpd=F)
  # mtext(side=3, text=title, font=2, line=2, cex=0.5)
  # if(axes.labels){
      # mtext(side=2, "Resource", line=0.5, cex=1)
      # mtext(side=3, "Consumer", line=0.5, cex=1)
  # }
  # if(diag.line==T)
    # lines(1:S, S:1, lty="dashed")
# }






## Make N completely random food web with S species and L links
## The output is a matrix if N=1 or a list of matrices if N>1
Random.model <- function(S, L, N=1){
    if(N==1)
        web <- matrix(c(rep(1, L),
                         rep(0, S^2-L))[order(runif(S^2))],
                      S, S)
    if(N>1){
        web <- list()
        for(i in 1:N){
            web[[i]] <- matrix(c(rep(1, L),
                            rep(0, S^2-L))[order(runif(S^2))],
                               S, S)
        }
    }
    web
}

## Make N cascade food webs with S species and L links
## The output is a matrix if N=1 or a list of matrices if N>1
## The cascade model is that of Cohen et al
Cascade.model <- function(S, L, N=1){
    if(N==1){
        web <- matrix(0, S, S)
        web[upper.tri(web)] <- c(rep(1, L), rep(0, (S^2-S)/2-L))[order(runif((S^2-S)/2))]
        dimnames(web) <- list(1:length(web[,1]), 1:length(web[,1]))
    }
    if(N>1){
        web <- list()
        for(i in 1:N){
            web[[i]] <- matrix(0, S, S)
            web[[i]][upper.tri(web[[i]])] <- c(rep(1, L), rep(0, (S^2-S)/2-L))[order(runif((S^2-S)/2))]
            dimnames(web[[i]]) <- list(1:length(web[[i]][,1]), 1:length(web[[i]][,1]))
        }
    }
    web
}

## Make N niche model food web with S species and L links
## The output is a matrix if N=1 or a list of matrices if N>1
## Niche model of Williams and Martinez
## No check for desired connectance
Niche.model <- function(S, L, N=1){
  C <- L/S^2
  if(N==1){
      n <- sort(runif(S))
      beta <- (1 - 2 * C) / (2 * C)
      r <- n*(1 - (1 - runif(S))^(1/beta))
      c <- r/2 + runif(S) * (n - r/2)
      web <- matrix(0,S,S)
      min.n <- c-r/2
      max.n <- c+r/2
      for(i in 1:S){
          diet <- c(1:S)[c(which(n>min.n[i]), which(n<max.n[i]))[duplicated(c(which(n>min.n[i]), which(n<max.n[i])))]]
          web[diet,i] <- 1
      }
      dimnames(web) <- list(1:length(web[,1]), 1:length(web[,1]))
  }
  if(N>1){
      web <- list()
      for(j in 1:N){
          n <- sort(runif(S))
          beta <- (1 - 2 * C) / (2 * C)
          r <- n*(1 - (1 - runif(S))^(1/beta))
          c <- r/2 + runif(S) * (n - r/2)
          web[[j]] <- matrix(0,S,S)
          min.n <- c-r/2
          max.n <- c+r/2
          for(i in 1:S){
              diet <- c(1:S)[c(which(n>min.n[i]), which(n<max.n[i]))[duplicated(c(which(n>min.n[i]), which(n<max.n[i])))]]
              web[[j]][diet,i] <- 1
          }
          dimnames(web[[j]]) <- list(1:length(web[[j]][,1]), 1:length(web[[j]][,1]))
      }
  }
  web
}


## Get the structural properties of a food web (predation matrix)
## or a list of predation matrices
## specify which stats with the which.stats argument:
## 1: Bottom, intermediate, top, unconnected, cannibal fractions
## 2: SD(generalism) and SD(vulnerability)
## 3: Maximum diet similarity
## 4: Trophic levels
Get.web.stats <- function(web, which.stats=1:4, real.web=NA){
    if(!is.list(web)){
        result <- c(S = length(web[,1]),
                    L = sum(web),
                    C = sum(web)/length(web[,1])^2)
        
        if(sum(which.stats==1)==1){
            BITUC <- Bottom.Intermediate.Top(web, proportion=T)$Proportions.of.each
            result <- c(result,
                        B = BITUC[1],
                        I = BITUC[2],
                        T = BITUC[3],
                        U = BITUC[4],
                        CB = Cannibals(web),
                        PCB = BITUC[5])
        }
        
        if(sum(which.stats==2)==1){
            result <- c(result,
                        Gensd = Gen.sd(web),
                        Vulsd = Vul.sd(web))
        }
        
        if(sum(which.stats==3)==1)
            result <- c(result, Maxsim=Maxsim(web))
        
        if(sum(which.stats==4)==1){

            TLs <- GetTL(web)
            if(is.na(TLs[1])){
                mean.TL <- NA
                max.TL <- NA
                sd.TL <- NA
            }
            if(!is.na(TLs[1])){
                mean.TL <- mean(TLs, na.rm=T)
                max.TL <- max(TLs, na.rm=T)
                sd.TL <- sd(TLs, na.rm=T)
            }
            
            result <- c(result,
                        mean.TL = mean.TL,
                        max.TL=max.TL,
                        sd.TL=sd.TL)                        

            
        }

        if(sum(which.stats==5)==1){

            ## move this to which.stats==1 set
            Num.H <- Num.herbivores(web) / length(web[,1])

            ## Move this to which.stats==4
            frac.omniv <- Fraction.omnivores(web)

            lev.omniv <- Level.omnivory(web)
            
            ## igraph package required
            library(igraph, lib.loc="~/mylibs/R")
            
            iweb <- graph.adjacency(web, mode="directed")
            clust.coef <- transitivity(iweb)
            char.path.length <- mean(shortest.paths(iweb))
            
            
            result <- c(result,
                        Num.H = Num.H,
                        Prop.om = frac.omniv,
                        Lev.om = lev.omniv,
                        Clust = clust.coef,
                        Cpl = char.path.length)
            
        }        
    }
    
    if(is.list(web)){
        for(i in 1:length(web)){
            
            t.result <- c(S = length(web[[i]][,1]),
                        L = sum(web[[i]]),
                        C = sum(web[[i]])/length(web[[i]][,1])^2)
            
            if(sum(which.stats==1)==1){
                BITUC <- Bottom.Intermediate.Top(web[[i]], proportion=T)$Proportions.of.each
                t.result <- c(t.result,
                            B = BITUC[1],
                            I = BITUC[2],
                            T = BITUC[3],
                            U = BITUC[4],
                            CB = Cannibals(web[[i]]),
                            PCB = BITUC[5])
            }
            
            if(sum(which.stats==2)==1){
                t.result <- c(t.result,
                            Gensd = Gen.sd(web[[i]]),
                            Vulsd = Vul.sd(web[[i]]))
            }
            
            if(sum(which.stats==3)==1)
                t.result <- c(t.result, Maxsim=Maxsim(web[[i]]))
            
            if(sum(which.stats==4)==1){

                
                TLs <- GetTL(web[[i]])
                if(is.na(TLs[1])){
                    mean.TL <- NA
                    max.TL <- NA
                    sd.TL <- NA
                }
                if(!is.na(TLs[1])){
                    mean.TL <- mean(TLs, na.rm=T)
                    max.TL <- max(TLs, na.rm=T)
                    sd.TL <- sd(TLs, na.rm=T)
                }
                
                t.result = c(t.result,
                    mean.TL = mean.TL,
                    max.TL=max.TL,
                    sd.TL=sd.TL)            
            }

            
        if(sum(which.stats==5)==1){

            ## move this to which.stats==1 set
            Num.H <- Num.herbivores(web[[i]])  / length(web[[i]][,1])

            ## Move this to which.stats==4            
            frac.omniv <- Fraction.omnivores(web[[i]])
            lev.omniv <- Level.omnivory(web[[i]])
            
            ## igraph package required
            library(igraph)
            iweb <- graph.adjacency(web[[i]], mode="directed")
            clust.coef <- transitivity(iweb)
            char.path.length <- mean(shortest.paths(iweb))
            
            
            t.result <- c(t.result,
                          Num.H = Num.H,
                          Prop.om = frac.omniv,
                          Lev.om = lev.omniv,                          
                          Clust = clust.coef,
                          Cpl = char.path.length)
            
        }



            if(i==1)
                result <- t.result
            if(i>1)
                result <- rbind(result, t.result)
        }
    }    
    result
}

Fraction.omnivores <- function(web) {
    TLs <- GetTL(web)
    non.int.TL <- web[,TLs %% 1 != 0]
    if(is.matrix(non.int.TL))
        frac.omniv <- sum(apply(non.int.TL, 2, sum) > 1)  / length(web[,1])
    if(is.vector(non.int.TL)) 
        frac.omniv <- (sum(non.int.TL) > 1)  / length(web[,1])
    frac.omniv
}

Level.omnivory <- function(web) {

    TLs <- GetTL(web)

    if( sum(is.na(TLs)) == length(TLs) )
       rr <- NA

    if( sum(is.na(TLs)) != length(TLs) ) {

        
        web.TLs <- matrix(rep(TLs, length(web[,1])), length(web[,1]), length(web[,1]))
        lo.pc <- numeric(length=length(web[,1]))
        for(i in 1:length(web[,1])) {
            tt <- web.TLs[web[,i]==1,i]
            if(length(tt)==0 | sum(!is.na(tt))==0 )
                lo.pc[i] = NA
            if(length(tt)>0 & sum(!is.na(tt))!=0)
                lo.pc[i] <- sd(tt)
        }
        rr <- mean(lo.pc, na.rm=T)
    }
    rr
}


## return the proportion of bottom, intermediate, top,
## unconnected and purely cannibalistic specie
Bottom.Intermediate.Top <- function(web, proportion=TRUE, check.web=FALSE){
    
  ## find the names and numbers of BIT, unconnected and pure cannibals
  names.all <- 1:length(web[1,])
  dimnames(web) <- list(names.all, names.all)
  names.Bottom <-  names.all[apply(web, 2, sum)==0 & apply(web, 1, sum)!=0]
  number.Bottom <- length(names.Bottom)
  names.Top <-   names.all[apply(web, 2, sum)!=0 & apply(web, 1, sum)==0]
  number.Top <- length(names.Top)
  names.Unconnected <- names.all[apply(web, 2, sum)==0 & apply(web, 1, sum)==0]
  number.Unconnected <- length(names.Unconnected)
  number.Pure.cannibals <- 0
  names.Pure.cannibals <- character(0)
  for(i in 1:length(web[,1]))
    if(web[i,i]!=0 && sum(web[-i,i]+web[i,-i])==0){
      if(number.Pure.cannibals==0){
        names.Pure.cannibals <- dimnames(web)[[1]][i]
        number.Pure.cannibals <- 1
      }
      else{
        names.Pure.cannibals <- c(names.Pure.cannibals, dimnames(web)[[1]][i])
        number.Pure.cannibals <- number.Pure.cannibals + 1
      }
    }
  names.Intermediate <- dimnames(web)[[1]][is.na(match(dimnames(web)[[1]],
                                                      c(names.Bottom, names.Top, names.Unconnected, names.Pure.cannibals)))]
  number.Intermediate <- length(web[1,])-number.Bottom-number.Top-number.Unconnected-number.Pure.cannibals

  if(proportion)
    result <- list(Bottom=names.Bottom,
                   Intermediate=names.Intermediate,
                   Top=names.Top,
                   Unconnected=names.Unconnected,
                   Pure.cannibals=names.Pure.cannibals,
                   Proportions.of.each=c(number.Bottom, number.Intermediate, number.Top, number.Unconnected, number.Pure.cannibals)/length(web[1,]))
  if(!proportion)
    result <- list(Bottom=names.Bottom,
                   Intermediate=names.Intermediate,
                   Top=names.Top,
                   Unconnected=names.Unconnected,
                   Pure.cannibals=names.Pure.cannibals,
                   Proportions.of.each=c(number.Bottom, number.Intermediate, number.Top, number.Unconnected, number.Pure.cannibals))
  result
}

Num.herbivores <- function(web) {
    
    S <- length(web[,1])
    ## Find the rows/columns with basal species
    B.rows <-  c(1:S)[apply(web, 2, sum)==0 & apply(web, 1, sum)!=0]
    ## Find r/c wth species that only eat basal
    if(length(B.rows)==0)
        Nh <- NA

    if(length(B.rows)>0) {
        if( length(B.rows)>1 & (S-length(B.rows))>1 )
            Nh <- sum( apply(web[B.rows,], 2, function(x) sum(x!=0)>0) &
                      apply(web[-B.rows,], 2, function(x) sum(x!=0)==0) )
        
        if( length(B.rows)==1 & (S-length(B.rows))>1 )
            Nh <- sum( web[B.rows,]!=0  &
                      apply(web[-B.rows,], 2, function(x) sum(x!=0)==0) )
        
        if( length(B.rows)>1 & (S-length(B.rows))==1 )
            Nh <- sum( apply(web[B.rows,], 2, function(x) sum(x!=0)>0) &
                      web[-B.rows,]==0) 
    }
    Nh

}
    
    
Cannibals <- function(web)
  length(dimnames(web)[[1]][diag(web)==1]) / length(web[1,])

Gen.sd <- function(web)
	## corrected, cf. Thomas Barnum, 24.1.1013
	sd(colSums(web)/sum(web)/length(web[,1]))
    ##sd(colSums(web)/sum(web))

Vul.sd <- function(web)
	## corrected, cf. Thomas Barnum, 24.1.1013
	sd(rowSums(web)/sum(web)/length(web[,1]))
    ##sd(rowSums(web)/sum(web))

Maxsim <- function(web){
  sims <- matrix(0, length(web[,1]), length(web[,1]))
  for(i in 1:length(web[,1]))
    for(j in 1:length(web[,1]))
      sims[i,j] <- T.sim.ij(web, i, j)
  diag(sims) <- NA
  mean(apply(sims, 1, function(x) max(x[!is.na(x)])))
}

## used by Maxsim
T.sim.ij <- function(web, i, j){
  same <- sum(web[i,] & web[j,]) + sum(web[,i] & web[,j])
  total <- sum(web[i,] | web[j,]) + sum(web[,i] | web[,j])
  same / total
}

GetTL <- function(web){
    
    ## takes predation matrix with consumers in columns
    ## identify the columns with basal species
    tweb <- t(web)

    ## make the rows add to one
    rs <- rowSums(tweb)
    for(i in 1:length(tweb[,1]))
        tweb[i,tweb[i,]==1] = 1/rs[i]

    nb.TL <- try(solve(diag(length(tweb[,1])) - tweb), T)

    if(class(nb.TL)=="try-error")
        nbTL <- rep(NA, length(tweb[,1]))

    if(class(nb.TL)!="try-error")
        nbTL <- rowSums(nb.TL)

    nbTL
    
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

## this can handle multiple prey columns, but blanks must be zeros
## the range argument is whether the list information is such that the second and third column contain the ranges
## in this case names must be numeric
List.to.matrix <- function(web.list, predator.first=TRUE){

 
  if(length(web.list[1,])==2){
    
    ## get the species names
    species <- c(as.character(web.list[,1]), as.character(web.list[,2]))
    species.names <- sort(unique(species[!is.na(species)]))
    
    number.of.species <- length(species.names)
    web.matrix <- matrix(0, number.of.species, number.of.species)
    dimnames(web.matrix) <- list(species.names, species.names)

    
    if(predator.first){
      for(i in 1:length(web.list[,1])){
          web.matrix[!is.na(match(dimnames(web.matrix)[[1]], web.list[i,2])),
                     !is.na(match(dimnames(web.matrix)[[2]], web.list[i,1]))] <- 1
        }


    }
      
      
    if(!predator.first)
      stop("Not completed the prey first non-range function")
  }
  
  if(length(web.list[1,])==3){


    ## get the species names
    species <- c(web.list[,1])
    for(i in 1:length(web.list[,1])){
      if(is.na(web.list[i,3]))
        species <- c(species, web.list[i,2])
      if(!is.na(web.list[i,3]))
        species <- c(species, web.list[i,2]:web.list[i,3])
    }
    species.names <- sort(unique(species[!is.na(species)]))
    
    number.of.species <- length(species.names)
    web.matrix <- matrix(0, number.of.species, number.of.species)
    dimnames(web.matrix) <- list(species.names, species.names)
    
    if(!is.numeric(species.names))
      stop("Species names must be numeric in range type web lists")

    if(predator.first)
      for(i in 1:length(web.list[,1])){
        if(is.na(web.list[i,3]))
          web.matrix[!is.na(match(dimnames(web.matrix)[[1]], web.list[i,2])),
                     !is.na(match(dimnames(web.matrix)[[2]], web.list[i,1]))] <- 1
        if(!is.na(web.list[i,3])){
          prey.links <- web.list[i,2]:web.list[i,3]
          for(p in prey.links)
            web.matrix[!is.na(match(dimnames(web.matrix)[[1]], p)),
                       !is.na(match(dimnames(web.matrix)[[2]], web.list[i,1]))] <- 1
        }
      }

    if(!predator.first)
      stop("Not completed the prey first function")
  }
  web.matrix
}




Draw.net <- function(webmat, ylims=c(0, 20)) {
     links <- matrix(as.numeric(Matrix.to.list(webmat)), sum(webmat), 2)
    spp <- as.numeric(dimnames(webmat)[[1]])
    add.spp <- c(1:40)[is.na(match(1:40, spp))]
    webmat <- rbind(webmat, matrix(0, length(add.spp), length(webmat[1,])))
    webmat <- cbind(webmat, matrix(0, length(webmat[,1]), length(add.spp)))
    dimnames(webmat) <- list(c(spp, add.spp),
                             c(spp, add.spp))
    webmat <- webmat[order(as.numeric(dimnames(webmat)[[1]])),
                     order(as.numeric(dimnames(webmat)[[1]]))]
    nodes <- data.frame(species=dimnames(webmat)[[1]])
    nodes <- transform(nodes, ypos=GetTL(webmat))
    nodes <- transform(nodes, xpos=NA)
    
    max.per.tl <- 0
    for(i in 1:ceiling(max(nodes$ypos))) {
        these <- nodes$ypos>=i & nodes$ypos<i+1
        if(sum(these)>max.per.tl) max.per.tl=sum(these)
    }
    intrvl <- 1/max.per.tl
    for(i in 1:ceiling(max(nodes$ypos))) {
        these <- nodes$ypos>=i & nodes$ypos<i+1
        xtemp <- seq(from=0, by=intrvl, length=sum(these))
        xtemp <- xtemp-mean(xtemp) + 0.5
        nodes$xpos[these] <- xtemp
    }
    
    plot(nodes$xpos, nodes$ypos, type="p",
         axes=F, ann=F, xlim=c(0,1), ylim=ylims)
    arrows(nodes$xpos[links[,1]], nodes$ypos[links[,1]],
           nodes$xpos[links[,2]], nodes$ypos[links[,2]],
           code=0, col="#00000022")
    ##points(nodes$xpos, nodes$ypos, pch=19, col="#ffffffff", cex=2)
    points(nodes$xpos, nodes$ypos, pch=21, bg="#22222211", cex=1,
           col="#22222211")
}


