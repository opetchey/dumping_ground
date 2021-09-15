

  
Ratio.allometric.EHL <- function(M,
                           e,
                           r.a, r.b,
                           a, ai, aj,
                           n, ni=-3/4){

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

            p <- P[,j]
            
            if(sum(p>0)==1)
                web[which(p>0),j] <- 1
                
            if(sum(p>0)>1){
            
                ## ordering of p required

                order.by.p <- order(p, decreasing=T)
                p <- p[order.by.p]
                Lj <- EHL[[3]][,j][order.by.p]
                hj <- EHL[[2]][,j][order.by.p]
                Ej <- EHL[[1]][order.by.p]
                
                cumulative.profit <- cumsum(Ej * Lj) / (1 + cumsum( Lj * hj))
                
                dj <- max(which(cumulative.profit==max(cumulative.profit)))
                
                web[,j] <- c(rep(1, dj), rep(0, S-dj))[order(order.by.p)]

                overall.energy[j] <- cumulative.profit[dj]

                energies <- c(Ej * Lj)[1:sum(web[,j])] / (1 + cumsum( Lj * hj)[sum(web[,j])]) 
                all.energies <- c(energies, rep(0, S-length(energies)))
                
                per.species.energy[,j] <- all.energies[order(order.by.p)]
                
            }
        }
    }
    
    if(is.vector(EHL[[3]])){
        for(j in 1:S){
            
            if(sum(p>0)==1)
                web[which(p>0),j] <- 1
            
            if(sum(p>0)>1){
                
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
                
                overall.energy[j] <- cumulative.profit[dj]    
                
                energies <- c(Ej * Lj)[1:sum(web[,j])] / (1 + cumsum( Lj * hj)[sum(web[,j])]) 
                all.energies <- c(energies, rep(0, S-length(energies)))
                
                per.species.energy[,j] <- all.energies[order(order.by.p)]
                
            }
        }
    }
    if(energy.intake)
            result <- list(web=web, overall.flux=overall.energy, per.species.flux=per.species.energy)
    else
        result <- web
    result
}



fit.ratio.C = function(a, x){

    e <- x[[1]]
    n <- x[[2]]
    ni <- x[[3]]
    ai = x[[4]]
    aj = x[[5]]
    r.a = x[[6]]
    r.b = x[[7]]
    M = x[[8]]
    S=x[[9]]
    target.C <- x[[10]]
    real.web <- x[[11]]

    
    a <- 10^a

    EHL <- Ratio.allometric.EHL(M=M,
                                e=e,
                                a=a, ai=ai, aj=aj,
                                n=n, ni=ni,
                                r.a=r.a, r.b=r.b)
    web <- Get.web(EHL)
    C=sum(web)/S^2                                             
    ans.wer = abs(target.C - C)
    #print(C)
    ans.wer
}

get.ratio.C <- function(a.vector, x){

    e <- x[[1]]
    n <- x[[2]]
    ni <- x[[3]]
    ai = x[[4]]
    aj = x[[5]]
    r.a = x[[6]]
    r.b = x[[7]]
    M = x[[8]]
    S=x[[9]]
    target.C <- x[[10]]
    real.web <- x[[11]]
    
    C = numeric(length=length(a.vector))
    for(i in 1:length(a.vector)){
        a = 10^a.vector[i]

        EHL <- Ratio.allometric.EHL(M=M,
                                e=e,
                                a=a, ai=ai, aj=aj,
                                n=n, ni=ni,
                                r.a=r.a, r.b=r.b)

        web <- Get.web(EHL)
        C[i] = sum(web)/S^2                                             
    }
    C
}    


get.ratio.a <- function(x){

    e <- x[[1]]
    n <- x[[2]]
    ni <- x[[3]]
    ai = x[[4]]
    aj = x[[5]]
    r.a = x[[6]]
    r.b = x[[7]]
    M = x[[8]]
    S=x[[9]]
    target.C <- x[[10]]
    real.web <- x[[11]]
    
    last.a <- 0
    interval <- 0.5
    flag=F

    ## first check if web has minimum connectance
    if(target.C<(1/S))
        a = 10^last.a
    if(!target.C<(1/S)){
        
        while(flag==F){
            last.C <- get.ratio.C(last.a, x)
            if(last.C<target.C)
                next.a <- last.a-interval
            if(last.C>target.C)
                next.a <- last.a+interval
            if(last.C==target.C){
                next.a <- last.a-1
                last.a <- last.a+1
                flag=T
            }   
            next.C <- get.ratio.C(next.a, x)
            if(last.a<next.a & last.C>target.C & next.C<target.C)
                flag=T
            if(last.a>next.a & last.C<target.C & next.C>target.C)
                flag=T
            if(flag==F)
                last.a = next.a
        }    
        a = 10^optimise(fit.ratio.C, range(c(last.a, next.a)), x=x)[[1]]
    }
    a
}

ratio.power <- function(opt, x, optimizer, opt.these=c("ai", "aj", "r.b")){

    poss.opt <- c("ai", "aj", "r.b")
    
    xopt.pars <- c(0,0,0)

    for(i in 1:length(opt.these))
        xopt.pars[match(opt.these[i], poss.opt)] <- opt[i]
    if(is.list(xopt.pars))
        xopt.pars <- unlist(xopt.pars)
    ai = xopt.pars[1]
    aj = xopt.pars[2]
    r.b = xopt.pars[3]

    e=x[[1]]
    n=x[[2]]
    ni=x[[3]]
    r.a=x[[4]]
    M=x[[5]]
    S=x[[6]]
    target.C <- x[[7]]
    real.web <- x[[8]]

    parms <- list(e, n, ni, ai, aj, r.a, r.b, M, S, target.C, real.web)
    
    ## check to make sure target.C is possible given r.b
    a <- 1
    EHL <- Ratio.allometric.EHL(M=M,
                                e=e,
                                a=a, ai=ai, aj=aj,
                                n=n, ni=ni,
                                r.a=r.a, r.b=r.b)

    ##print(sum(EHL[[2]]!=Inf))
    
    if(sum(EHL[[2]]!=Inf)>=(target.C*S^2)){
        a = get.ratio.a(parms)
        EHL <- Ratio.allometric.EHL(M=M,
                                    e=e,
                                    a=a, ai=ai, aj=aj,
                                    n=n, ni=ni,
                                    r.a=r.a, r.b=r.b)
        web <- Get.web(EHL)
        C=sum(web)/S^2                                             
        if(abs(target.C-C)>0.05)
            ratio.yy = -1
        else{            
            ratio.yy <- Compare.links(real.web, web)
        }
    }
    if(sum(EHL[[2]]!=Inf)<(target.C*S^2))
        ratio.yy = -1    
    if(optimizer)
        result = ratio.yy
    if(!optimizer)
            result = c(ratio.yy, a)
    result
}





NM.ratio <- function(all.web.info, init.pars){

    ratio.initial.pars <- init.pars
    
    real.web <- all.web.info$predation.matrix
    M <- all.web.info$species.sizes
    S <- dim(real.web)[1]
    target.C <- sum(real.web)/S^2    

    e <- 1
    n <- 1
    ni <- -3/4
    r.a = 1
    parms <- list(e=e, n=n, ni=ni, r.a, M=M, S=S, target.C=target.C, real.web)
    ##ratio.power(ratio.initial.pars[ip,], parms)
    
    best = 0

    ## no idea why this next line is here
    ##ratio.initial.pars[,3] <- 1
    
    for(ip in 1:length(ratio.initial.pars[,1])){

        print(ratio.initial.pars[ip,])

        real.web <- all.web.info$predation.matrix
        M <- all.web.info$species.sizes
        S <- dim(real.web)[1]
        target.C <- sum(real.web)/S^2    
        
        e <- 1
        n <- 1
        ni <- -3/4
        r.a = 1
        parms <- list(e=e, n=n, ni=ni, r.a, M=M, S=S, target.C=target.C, real.web)
        ##ratio.power(ratio.initial.pars[ip,], parms)
                
        o.p <- optim(ratio.initial.pars[ip,], ratio.power, control=list(fnscale=-1, trace=1), x=parms, optimizer=T)
        
        if(o.p$value!=-1){
            ## a <- 1
            ##         EHL <- Ratio.allometric.EHL(M=M,
            ##                                     e=e,
            ##                                     a=a, ai=o.p$par[1], aj=o.p$par[2],
            ##                                     n=n, ni=ni,
            ##                                     r.a=r.a, r.b=o.p$par[3])
            if(o.p$value>best){
                best=o.p$value
                ##if(sum(EHL[[2]]!=Inf)>=(target.C*S^2)){
                parms <- list(e, n, ni, ai=o.p$par[1], aj=o.p$par[2], r.a, r.b=o.p$par[3], M, S=S, target.C=target.C, real.web)
                a = get.ratio.a(parms)
                best.EHL <- Ratio.allometric.EHL(M=M,
                                            e=e,
                                            a=a, ai=o.p$par[1], aj=o.p$par[2],
                                            n=n, ni=ni,
                                            r.a=r.a, r.b=o.p$par[3])
                best.web <- Get.web(best.EHL) 
                optim.power.pars <- c(e=e, n=n, ni=ni, a=a, ai=o.p$par[1], aj=o.p$par[2], r.a=r.a, r.b=o.p$par[3])

            }
        }
    }
    if(best==0){
        ##if(sum(EHL[[2]]!=Inf)<(target.C*S^2))
        optim.power.pars <- rep(NA, 8)
        best.EHL=NA
        best.web=NA
    }
    list(power=best, pars=optim.power.pars, EHL=best.EHL, web=best.web)
}


NM.ratio.comp <- function(all.web.info){

    
    
    ratio.initial.pars <- read.csv("~/allometric.web/data/NM.optim/ratio.initial.pars.txt", header=F)
    

    real.web <- all.web.info$predation.matrix
    M <- all.web.info$species.sizes
    S <- dim(real.web)[1]
    target.C <- sum(real.web)/S^2    

    e <- 1
    n <- 1
    ni <- -3/4
    r.a = 1
    parms <- list(e=e, n=n, ni=ni, r.a, M=M, S=S, target.C=target.C, real.web)
    ##ratio.power(ratio.initial.pars[ip,], parms)
    
    best = 0

    opt.pars <- c("ai", "aj", "r.b")

    
    par.holds <- cbind(rep(c(0,1), each=4, length.out=8),
                       rep(c(0,1), each=2, length.out=8),
                       rep(c(0,1), each=1, length.out=8))

    best.by.ph <- list()
    
    for(ph in 2:length(par.holds[,1])){
        print(ph)
        best = 0

        opt.these <- opt.pars[par.holds[ph,]==1]

        
        for(ip in 1:length(ratio.initial.pars[,1])){
            
            if(length(opt.these)>1)
               o.p <- optim(ratio.initial.pars[ip, par.holds[ph,]==1],
                            ratio.power,
                            control=list(fnscale=-1, trace=0),
                            x=parms, optimizer=T,
                            opt.these=opt.these)
            if(length(opt.these)==1){
                o.p <- list(par=NA, value=NA)
                opt.1d <- optimise(ratio.power,
                                   lower=c(0.00001), upper=1,
                                   maximum=T,
                                   x=parms, optimizer=T,
                                   opt.these=opt.these)
                o.p$par <- opt.1d$maximum
                o.p$value <- opt.1d$objective
            }
                
            poss.opt <- opt.pars
    
            opt1.pars <- c(0,0,0)
            
            for(i in 1:length(opt.these))
                opt1.pars[match(opt.these[i], poss.opt)] <- o.p$par[i]
            
            ai = opt1.pars[1]
            aj = opt1.pars[2]
            r.b = opt1.pars[3]

            
            
            if(o.p$value!=-1){
                if(o.p$value>best){
                    best=o.p$value
                    best.parms <- list(e, n, ni, ai=ai, aj=aj, r.a, r.b=r.b, M, S=S, target.C=target.C, real.web)
                    a = get.ratio.a(best.parms)
                    best.EHL <- Ratio.allometric.EHL(M=M,
                                                     e=e,
                                                     a=a, ai=ai, aj=aj,
                                                     n=n, ni=ni,
                                                     r.a=r.a, r.b=r.b)
                    best.web <- Get.web(best.EHL) 
                    optim.power.pars <- c(e=e, n=n, ni=ni, a=a, ai=ai, aj=aj, r.a=r.a, r.b=r.b)
                    
                }
            }
        }
        if(best==0){
            ##if(sum(EHL[[2]]!=Inf)<(target.C*S^2))
            optim.power.pars <- rep(NA, 8)
            best.EHL=NA
            best.web=NA
        }
        
        best.by.ph[[ph]] <- list(best, par.holds[ph,], best.web)
        
    }
    best.by.ph

}


Compare.links <- function(real.web, model.web){
  result <- sum(real.web==1 & model.web==1)/sum(model.web)
  result
}
