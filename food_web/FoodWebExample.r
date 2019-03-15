rm(list=ls())

## First, source the standard food web functions file, or paste it all into R
source("standard.food.web.functions.r")

## make some random, cascade, and niche food webs
S = 40     ## set species richness
C = 0.1    ## set connectance
N = 1000     ## set the number of replicate webs to make
L = S^2*C  ## calculate number of links from S and C

random.webs <- Random.model(S, L, N)
cascade.webs <- Cascade.model(S, L, N)
niche.webs <- Niche.model(S, L, N)

## plot the webs
par(mar=c(2,2,2,2))
layout(matrix(1:4, 2, 2), respect=T)
for(i in 1:4){
    Plot.matrix(random.webs[[i]])
    box()
}
for(i in 1:4){
    Plot.matrix(cascade.webs[[i]])
    box()
}
for(i in 1:4){
    Plot.matrix(niche.webs[[i]])
    box()
}

## get the structural properties
random.structure <- Get.web.stats(random.webs)
cascade.structure <- Get.web.stats(cascade.webs)
niche.structure <- Get.web.stats(niche.webs)

## some histograms of diet similarity, for example
par(mar=c(5,4,4,2))
prop <- "Maxsim"
prop.range <- range(c(random.structure[,prop],
                      cascade.structure[,prop],
                      niche.structure[,prop]))
prop.bins <- seq(prop.range[1], prop.range[2], length=10)
layout(matrix(1:3, 3, 1), respect=T)
hist(random.structure[,prop], main="Random", xlab=prop, breaks=prop.bins)
hist(cascade.structure[,prop], main="Cascade", xlab=prop, breaks=prop.bins)
hist(niche.structure[,prop], main="Niche", xlab=prop, breaks=prop.bins)
## Big differences between models!
