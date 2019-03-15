# Author: Owen Petchey (Please acknowledge as appropriate)
source('~/core.R.functions/core_R_functions.r', chdir = TRUE)

s <- 26
species.names <- letters[1:s]
comp <- Random.comp(species.names, 5, 1:20)
traits <- matrix(runif(s), s, 1)
dimnames(traits)[[1]] <- species.names
traits
dists <- dist(traits)
clust <- hclust(dists)
xtree <- Xtree(clust)

FD <- Getlength(xtree, comp)
plot(FD)

VAR <- 

x <- comp$new.species[comp$community==30]
x
VAR <- tapply(comp[,2], comp[,1], function(x) var(traits[!is.na(match(dimnames(traits)[[1]], x))]))

plot(FD[,2], VAR)




x1 <- c(1, 2, 3, 4, 5, 6)
x2 <- c(1, 2, 3, 4, 5, 6, 6)
var(x1)
var(x2)
Getlength(Xtree(hclust(dist(x1))))
Getlength(Xtree(hclust(dist(x2))))