#

library(shiny)
library(igraph)
library(NetIndices)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Allometric diet breadth model"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       numericInput("ran_seed",
                  "Random number seed:",
                  min = 1,
                  max = 50000,
                  value = 1),
       sliderInput("num_S",
                   "Number of species:",
                   min = 10,
                   max = 500,
                   value = 50),
       sliderInput("mean_BM",
                   "Mean body mass:",
                   min = 1,
                   max = 50,
                   value = 10),
       sliderInput("sd_log_BM",
                   "Standard deviation of log normal body mass distribution:",
                   min = 1,
                   max = 10,
                   value = 5),
       sliderInput("a",
                   "Log10(Attack rate - mass scaling constant):",
                   min = -10,
                   max = 4,
                   value = -2),
       sliderInput("ai",
                   "Prey mass - attack rate scaling exponent:",
                   min = 0.5,
                   max = 1,
                   value = 0.5),
       sliderInput("aj",
                   "Predator mass attack rate scaling exponent:",
                   min = 0.5,
                   max = 1,
                   value = 0.5),
       sliderInput("r.a",
                   "Handling time - mass scaling constant:",
                   min = 1,
                   max = 2,
                   value = 1),
       sliderInput("r.b",
                   "Critical predator - prey mass ratio:",
                   min = 0.01,
                   max = 10,
                   value = 0.1),
       sliderInput("e",
                   "Prey energy content - mass scaling constant:",
                   min = 1,
                   max = 2,
                   value = 1),
       sliderInput("ei",
                   "Prey energy content - mass scaling exponent:",
                   min = 1,
                   max = 1,
                   value = 1),
       sliderInput("n",
                   "Prey abundance - mass scaling constant:",
                   min = 1,
                   max = 1,
                   value = 1),
       sliderInput("ni",
                   "Prey abundance - mass scaling exponent:",
                   min = -1,
                   max = 0,
                   value = -0.75)
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      withMathJax(),
      includeMarkdown("intro.md"),
      plotOutput("MPlot", width="200px", height="200px"),
      plotOutput("fwmatrixPlot", width="400px", height="400px"),
      plotOutput("fwgraphPlot", width="400px", height="400px")

    )
  )
))
