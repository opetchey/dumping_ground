#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)
library(tidyverse)

source("new.adbm.r")
source("Plot.matrix.r")

## for testing
rm(list=ls())
ref_seed <- 1
ref_num_S <- 20
ref_mean_BM <- 10
ref_sd_log_BM <- 1
ref_a <- 1
ref_ai <- 0.5
ref_aj <- 0.5
ref_r.a <- 1
ref_r.b <- 2
ref_e <- 1
ref_ei <- 1
ref_n <- 1
ref_ni <- -0.75

set.seed(ref_seed <- 1)
M <- sort(rlnorm(ref_num_S, ref_mean_BM, ref_sd_log_BM))

EHL <- Ratio.allometric.EHL(M=M,
                            e=ref_e,
                            r.a=ref_r.a, r.b=ref_r.b,
                            a=10^ref_a, ai=ref_ai, aj=ref_aj,
                            n=ref_n, ni=ref_ni)
median(EHL[[3]])
webout <- Get.web(EHL)
ref_L <- median(EHL[[3]])


# Define server logic
shinyServer(function(input, output) {
  
  M <- reactive({
    set.seed(input$ran_seed)
    M <- sort(rlnorm(input$num_S, input$mean_BM, input$sd_log_BM))
    return(M)
  })
  
  web <- reactive({
  
    EHL <- Ratio.allometric.EHL(M=M(),
                                   e=input$e,
                                   r.a=input$r.a, r.b=input$r.b,
                                   a=10^ref_a, ai=input$ai, aj=input$aj,
                                   n=input$n, ni=input$ni)
    new_L <- median(EHL[[3]])
    a_factor <- ref_L/new_L
    EHL <- Ratio.allometric.EHL(M=M(),
                                e=input$e,
                                r.a=input$r.a, r.b=input$r.b,
                                a=10^input$a*a_factor, ai=input$ai, aj=input$aj,
                                n=input$n, ni=input$ni)
    
    webout <- Get.web(EHL)
    return(webout)
  })
  
  output$MPlot <- renderPlot({
    ggplot(mapping=aes(x=M())) + geom_histogram() +
      xlab("Body mass")
  })
  
  output$connectance <- renderText(print(paste("Connectance =", round(sum(web())/input$num_S^2,2))))
  
  output$fwmatrixPlot <- renderPlot({
    Plot.matrix(web())
    box()
  })
  
  output$fwgraphPlot <- renderPlot({
    ## Calculate trophic level... funky when there is no strict hierarchy
    TL <- TrophInd(web())$TL-1
    oo <- order(TL) ## save order, so we can later reorder the food web
    ## Chunk to make x_pos with width according to num spp per TL
    d_TL <- round(TL)
    spp_per_TL <- table(d_TL)
    x_pos <- rep(NA, length=input$num_S)
    st <- cumsum(spp_per_TL)
    x_pos[1:st[1]] <- 1:st[1] - mean(1:st[1])
    for(i in 2:length(spp_per_TL))
      x_pos[(st[i-1]:(st[i]-1))+1] <- st[i-1]:(st[i]-1)-mean(st[i-1]:(st[i]-1))
    ## make and fill the layout matrix
    lay<-matrix(nrow=input$num_S, ncol=2)
    #lay[,1] <- 1:input$num_S
    lay[,1] <- x_pos
    lay[,2] <- (TrophInd(web())$TL-1)
    ## reorder the web to correspond to layout
    oo_web <- web()[oo,oo]
    ## convert web to igraph object
    gg <- graph_from_adjacency_matrix(web())
    ## plot
    par(mar=c(.1,.1,.1,.1))
    plot.igraph(gg,layout=lay,
                vertex.label=NA,vertex.size=40,
                edge.arrow.size=.5,edge.width=.5,
                rescale=FALSE,
                ylim=c(0,20), xlim=c(-10,10),
                frame=TRUE)
  })
  
  
})
