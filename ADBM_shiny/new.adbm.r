
Ratio.allometric.EHL <- function(M,
                           e,
                           r.a, r.b,
                           a, ai, aj,
                           n, ni=-3/4){

    Mi = M
    Mj = M
    
  get.h <- function(Mi, Mj, r.a, r.b)
    ifelse((r.b-Mi/Mj)>0,
           r.a/(r.b-Mi/Mj),
           Inf)
  
  ## in matrix H resources are rows and consumers are columns


  if(!r.b==0)
      H <- outer(M, M, get.h, r.a, r.b)
  if(r.b==0)
      H = matrix(r.a, length(M),length(M))

  

  
  ## ENCOUNTER RATES: consumer - resource mass specific encounter rates
  get.a <- function(Mi, Mj,
                    a, ai, aj)
    a * Mi^ai * Mj^aj
  A <- outer(M, M, get.a,
             a=a, ai=ai, aj=aj)
  
  ## not sure what this does:
  # if(length(absxxx)>1) {
  #     ##print(x)
  #     L <- A * absxxx
  # }
  # if(length(absxxx)==1)
  #     L <- A* n*Mi^ni
  

  L <- A* n*M^ni
  
  if(sum(order(M)==1:length(M))!=length(M))
    stop("Body sizes not sorted")
  
  ## energy values
  E <- e*M
  
  list(E=E, H=H, L=L)
  
}


Get.web <- function(EHL, energy.intake=F){

  ##E <- EHL[[1]]
  ##H <- EHL[[2]]
  ##L <- EHL[[3]]
  S <- length(EHL[[1]])
  
  web <- matrix(0, S, S)
  overall.energy <- numeric(S)
  per.species.energy <- matrix(0, S, S)
  
  ## in matrix P, columns are cosumers and contain profit of that consumer
  ## feeding on each prey (row)
  P <- EHL[[1]]/EHL[[2]]

  ## split code depending on whether encounter rates are predator specific or not
  if(is.matrix(EHL[[3]])){
    for(j in 1:S){
      
      ## ordering of p required
      p <- P[,j]
      order.by.p <- order(p, decreasing=T)
      p <- p[order.by.p]
      Lj <- EHL[[3]][,j][order.by.p]
      hj <- EHL[[2]][,j][order.by.p]
      Ej <- EHL[[1]][order.by.p]
      
      cumulative.profit <- cumsum(Ej * Lj) / (1 + cumsum( Lj * hj))
      ##cumulative.profit[length(E)] <- NA

      if(!all(p==0)){
        web[,j] <- c(1, cumulative.profit[1:(length(EHL[[1]])-1)] < p[2:length(EHL[[1]])])[order(order.by.p)]
        overall.energy[j] <- cumulative.profit[sum(web[,j])]
      }
      energies <- c(Ej * Lj)[1:sum(web[,j])] / (1 + cumsum( Lj * hj)[sum(web[,j])]) 
      all.energies <- c(energies, rep(0, S-length(energies)))
      
      per.species.energy[,j] <- all.energies[order(order.by.p)]
       
    }
  }
  if(is.vector(EHL[[3]])){
    for(j in 1:S){
      
      ## ordering of p required
      p <- P[,j]
      order.by.p <- order(p, decreasing=T)
      p <- p[order.by.p]
      Lj <- EHL[[3]][order.by.p]
      hj <- EHL[[2]][,j][order.by.p]
      Ej <- EHL[[1]][order.by.p]
      
      cumulative.profit <- cumsum(Ej * Lj) / (1 + cumsum( Lj * hj))
   
      dj <- max(which(cumulative.profit==max(cumulative.profit)))
      web[,j] <- c(rep(1, dj), rep(0, S-dj))[order(order.by.p)]

      overall.energy[j] <- cumulative.profit[sum(web[,j])]    

      energies <- c(Ej * Lj)[1:sum(web[,j])] / (1 + cumsum( Lj * hj)[sum(web[,j])]) 
      all.energies <- c(energies, rep(0, S-length(energies)))
      
      per.species.energy[,j] <- all.energies[order(order.by.p)]
    
    }
  }

  #web[,!these] <- 0
  
  if(energy.intake)
    result <- list(web=web, overall.flux=overall.energy, per.species.flux=per.species.energy)
  else
    result <- web
  result
}
