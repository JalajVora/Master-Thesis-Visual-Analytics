#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

fluidPage(
  
  headerPanel("Semi-automated pattern detection in tabular data with weak hierarchies"),
  
  sidebarLayout(
    sidebarPanel(
      # "Story Classification" = "stry_clss",
      helpText("Option to select a dataset to explore"),
      selectInput('select_dataset', 'Select Dataset:', c("Story Classification" = "stry_clss",
                                                         "Upper Triangle" = "uppr_trngl",
                                                         "Lower Triangle" = "lwr_trngl")),
    ),
    
    mainPanel(
      # fluidRow(
      #   column(3, DT::dataTableOutput('table', width = 500)),
      # ),
      tabsetPanel(
        tabPanel("t-SNE", 
                 sliderInput("n_iters", "Number of iterations", min = 500, max = 5000, value = 500), 
                 sliderInput("perplexity", "Perplexity", min = 5, max = 30, value = 5, step = 2),
                 hr(),
                 plotlyOutput("tsnePlot")
        ),
        tabPanel("BiClustering", selectInput('select_method', 'Select BiClustering Method:',
                                             c("BiMax" = "bimax", "BCCC" = "bccc")),
                 conditionalPanel(condition = "input.select_method == bimax",
                                  numericInput("n_biclstrs", "Number of Biclusters",
                                               value = 4, min = 1, max = 20, step = 1),
                                  br(),
                                  numericInput("minr", "Minimum number of rows",
                                               value = 4, min = 2, max = 100, step = 1),
                                  numericInput("mincol", "Minimum number of Columns",
                                               value = 4, min = 2, max = 50, step = 1)
                 ),
                 hr(),
                 plotlyOutput(outputId = "heat_map", width = "1000px", height = "900px"),
                 hr(), br(),
                 # plotOutput("bic.plot", width = "800px", height = "900px", hover = "values")
                 #          # plotlyOutput("bic.scatter", width = "800px", height = "900px"),
                 #          # hr(),
                 #          # plotOutput("biclustplot", width = "800px", height = "900px", hover = "values"),
        ),
      ),
    )
  )
)

