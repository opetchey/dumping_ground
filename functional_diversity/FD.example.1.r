# Author: Owen Petchey (Please acknowledge as appropriate)

# An example of using R to calculate FD of a complete community
# and then FD of the same community with a random species removed
# Notations corresponds with Petchey & Gaston (2002) Ecology Letters 4:402-411

# Some parameters and methods
Number.of.species <- 10
Number.of.traits  <- 3
Distance.method <- "euclidean"
Cluster.method <- "average"

# 1) Obtain a trait matrix (here of random numbers)
S <- matrix(rnorm(Number.of.species*Number.of.traits), Number.of.species, Number.of.traits)
rownames(S) <- letters[1:Number.of.species]

# 2) Obtain the distance matrix
#    Note: standardise the trait matrix if you wish
D <- dist(S, method=Distance.method)
## Use metric=Distance.method in S-Plus

# 3) Obtain the dendrogram
tree <- hclust(D, method=Cluster.method)
# plot it if you like using plot(tree)

# 4) Get the total branch length
#    i)   Make sure that the Xtree (below) has been pasted into R
#    ii)  Transform tree into xtree
xtree <- Xtree(tree)
#    iii) Get the branch presence/absence vector (i' == i.prime)
i.prime <- ifelse(colSums(xtree$H1)>0, 1, 0)
#    iv)  FD is the sum of the product of i.prime and h2.prime
FD <- sum(i.prime*xtree$h2.prime)
FD

# 5) And FD after a random species is removed
sub.i.prime <- ifelse(colSums(xtree$H1[sample(1:length(xtree$H1[,1]), length(xtree$H1[,1])-1),])>0, 1, 0)
FD.new <- sum(sub.i.prime*xtree$h2.prime)
FD.new
