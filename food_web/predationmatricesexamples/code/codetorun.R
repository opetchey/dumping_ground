## Below is some code for creating predation matrices.
## It accompanies the content of this web page, about plotting predation matrices:
## http://www.thetrophiclink.org/?p=298
## You should find this file of code in a folder containing a file of useful functions
## and with a folder containing the various datasets required.
## If you're using a mac, and put the folder "predationmatricesexamples" on your desktop
## everything should run fine,
## though the last chunk only if you install the package "animation".
## If you're running this on another machine, you'll need to fiddle with the paths.
## Any questions, contact Owen Petchey


## clear R
rm(list=ls())


## Read in some useful functions
source("~/Desktop/predationmatricesexamples/code/someusefulfunctions.r")



## Read in the example, Benguela pelagic food web (from Yodzis 1998)
## the format of the food web data here is one created by Owen
## the object is called all.web.info
load("~/Desktop/predationmatricesexamples/datasets/Benguela Pelagic.web.Rdata")
Plot.matrix(all.web.info$predation.matrix,
			point.cex=2,
			trait.cex=1,
			diag.line=T,
			traits=F,
			by.consumer=T,
			axes.labels=T,
			sp.pt.ch=NA,
			grid=T,
			box=T)





## Use the Niche.model function to make an example food web
test.food.web.matrix <- Niche.model(10, 20, 1)



##################################################################
##################################################################
## Simplest method for plotting a predation matrix
image(test.food.web.matrix)
##################################################################
##################################################################




##################################################################
##################################################################
## Similarly simple is a function Owen made
Plot.matrix(test.food.web.matrix,
			point.cex=2,
			trait.cex=1,
			diag.line=T,
			traits=F,
			by.consumer=T,
			axes.labels=T,
			sp.pt.ch=NA,
			grid=T,
			box=T)
##################################################################
##################################################################





##################################################################
##################################################################
## Doing it with cheddar
library(cheddar)
## first we have to get the data into cheddar
test.food.web.list <- as.data.frame(Matrix.to.list(test.food.web.matrix))
names(test.food.web.list) <- c("resource", "consumer")
test.food.web.list$resource <- letters[1:10][test.food.web.list$resource]
test.food.web.list$consumer <- letters[1:10][test.food.web.list$consumer]
cheddar.example.comm <- Community(nodes=data.frame(node=letters[1:10]),
		  properties=list(title="An example"),
		  trophic.links=test.food.web.list)
## now the plot
PlotPredationMatrix(cheddar.example.comm)
##################################################################
##################################################################




##################################################################
##################################################################
## Making a more complex plot, for example, a figure similar to that of
## Woodward et al, 2010, Adv. Ecol. Res., 43, 212

## Preliminaries
setwd("~/Desktop/predationmatricesexamples/datasets/")
load("species.average.web.Rdata")
load("NMRH.web.pred.act.Rdata")
web <- all.web.info$predation.matrix
p.web <- NM.RH.web$web
sizes <- all.web.info$species.sizes
abs <- all.web.info$species.abundance
s.names <- dimnames(web)[[1]]
load(file="species.size.ranges.Rdata")
mins <- size.ranges$mins
maxs <- size.ranges$maxs
means <- size.ranges$means
oo <- order(means[,2])

## Set up the graphing layout
layout(matrix(1:4, 2,2, byrow=T), widths=c(4,1), heights=c(1,4))

## Plot above the predation matrix
par(mar=c(0,8,1,0))
barplot(log10(abs), ylab="log abundance", ylim=c(0,4))

## Blank plot to the top left of the predation matrix
plot(1,1,ann=F,type="n",axes=F)

## Plot the predation matrix
par(mar=c(10,8.6,1,0.5))
Plot.matrix(web, point.cex=0.8, pt.col="black")
abline(h=1:29, col="grey", lty="dotted")
abline(v=1:29, col="grey", lty="dotted")
par(new=T)
Plot.matrix(p.web, point.cex=1.6, pt.col="grey")
par(new=T)
Plot.matrix(web, point.cex=0.8)
par(new=F)
box()
mtext(side=2, at=1:29, text=rev(s.names), las=1, line=1.1, font=3, cex=0.8)
mtext(side=1, at=1:29, text=s.names, las=2, line=1.1, font=3, cex=0.8)

## Plot to the right of the predation matrix
load(file="species.size.ranges.Rdata")
mins <- size.ranges$mins
maxs <- size.ranges$maxs
means <- size.ranges$means
oo <- order(means[,2])
par(mar=c(9,0,1,1))
plot(1,1, type="n",
     xlim=c(-9, 4),
     ylim=c(0, 28),
     ann=F, axes=F)
arrows(mins[oo,2], 1:28,
       maxs[oo,2], 1:28,
       length=0)
points(means[oo,2], 1:28, pch=1)
axis(1, at=c(-8, -4, 0, 4))
mtext(side=1, line=3,text="Log10(body mass)", cex=0.7)
##################################################################
##################################################################







##################################################################
##################################################################
## Code to make a movie of predation matrices.
## Doesn't produce exactly the same animation as on the web page.
## Tested only on a mac.
rm(list=ls())

library(animation)

## Read in some useful functions
source("~/Desktop/predationmatricesexamples/code/someusefulfunctions.r")

## read in the food web matrices
web.files <- dir("~/Desktop/predationmatricesexamples/datasets/websformovie")

## make the gif animation...
saveGIF(
  {
  	
  ## This counter exists because the code will plot 20 webs (matrices) on top of each other
  ## for each frame of the video.	
  twenty.counter <- 0

  for(file.counter in 1:length(web.files)) {    

	twenty.counter <- twenty.counter+1

	webmat <- read.csv(paste("~/Desktop/predationmatricesexamples/datasets/websformovie/", web.files[file.counter], sep=""), row.names=1)
    colr <- "#00000010"  
    
	if(twenty.counter==1) {
      par(new=F, mar=c(1,1,1,20))
      Plot.matrix(webmat, pt.col=colr, point.cex=1, grid=F)
      par(new=T, mar=c(1,20,1,1))
      Draw.net(webmat, c(0,25))                   
    }
                 
	if(twenty.counter>1) {         
      par(new=T, mar=c(1,1,1,20))
      Plot.matrix(webmat, pt.col=colr, point.cex=1, diag.line=F, box=F, grid=F)
      par(new=T, mar=c(1,20,1,1))
      Draw.net(webmat, c(0,25))                   

    }
  if(twenty.counter==20) twenty.counter <- 0
                          
  }
  },
  interval=0.25,
  ani.width=500,
  ani.height=200,
  loop=T
)
##################################################################
##################################################################




