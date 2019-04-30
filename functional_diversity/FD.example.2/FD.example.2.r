# Author: Owen Petchey (Please acknowledge as appropriate)

# An example of using R to calculate FD of different communities with different compositions
# Notations corresponds with Petchey & Gaston (2002) Ecology Letters 4:402-411

## The analyses use three data files that are only one way of arranging the data for analysis.
## Each was created in xl for convenience and saved as an xl file and
## as a comma delimited text file (extension .csv) because these are easily read into R
## Species are identified by a label that must correspond between the three data file.
## Communities are also.

## A species-trait matrix (species are rows, traits are columns)
## Here, the label that identifies species is in the first column (a through j)
species.traits <- read.csv("C:\\path\\to\\data\\files\\here\\species.traits.csv")
## species.traits <- read.csv("~/Desktop/web page/assets/fd/species.traits.csv")

## A matrix that describes which species are in each community
## Here, the label that identifies a species corresponds to the trait matrix
## and the labels that identify a community (first column; numbers) correspond to the
## community labels in the community.functioning file
community.composition <- read.csv("C:\\path\\to\\data\\files\\here\\community.composition.csv")
## community.composition <- read.csv("~/Desktop/web page/assets/fd/community.composition.csv")

## A matrix that describes the level of functioning (or whatever was measured)
## in each community
community.functioning <- read.csv("C:\\path\\to\\data\\files\\here\\community.functioning.csv")
## community.functioning <- read.csv("~/Desktop/web page/assets/fd/community.functioning.csv")

# Some methods
Distance.method <- "euclidean"
Cluster.method <- "average"

## Calculate the functional dendrogram for all species
## first put the species labels as the dimname of rows
dimnames(species.traits) <- list("species"=as.character(species.traits[,1]),"traits"=dimnames(species.traits)[[2]])
## Standardise the trait matrix if you wish (I haven't here for no particular reason.)
distances <- dist(species.traits[,-1]) ## the minus 1 removes the first column (species names)
tree <- hclust(distances)
xtree <- Xtree(tree)

## Then use the tapply function to calculate the FD of each community composition
## using the Getlength function (below)
FD <- tapply(community.composition[,2], community.composition[,1],
               function(x) Getlength(list(branchlengths=xtree$h2.prime,
                                          sppxbranch=xtree$H1[!is.na(
                                            match(dimnames(xtree$H1)[[1]],x)),])))

## or just let Getlength do the work!...
FD <- Getlength(xtree, community.composition)

## Then look to see if the measure of functioning correlates with FD
## NB: make sure the rows ordering matches ##### THIS IS REALLY IMPORTANT!!! #####
plot(FD, community.functioning[,2])

    
## New FD
Getlength <- function(xtree, comp=NA){
  if(!is.data.frame(comp))
      result <- Getlength.inner(xtree)
  if(is.data.frame(comp)){
      S <- tapply(comp[,2], comp[,1], function(x) length(x))
      FD <- tapply(comp[,2], comp[,1], function(x) Getlength.inner(list(xtree[[1]], xtree[[2]][!is.na(match(dimnames(xtree[[2]])[[1]], x)),]))) 
      FD <- FD/Getlength.inner(xtree)
      result <- data.frame(S=S, FD.new=FD)
  }
  result
}
Getlength.inner <- function(xtree){   
    if(!is.matrix(xtree[[2]]))
        result <- 0
    if(is.matrix(xtree[[2]]))
        result = sum(xtree[[1]][colSums(xtree[[2]]) != 0 & colSums(xtree[[2]]) < length(xtree[[2]][,1])])
    result
}
