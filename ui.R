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
library(ComplexHeatmap)
library(InteractiveComplexHeatmap)

fluidPage(
  
  headerPanel("Semi-automated pattern detection in tabular data with weak hierarchies"),
  
  sidebarLayout(
    sidebarPanel(
      # "Story Classification" = "stry_clss",
      helpText("Option to select a dataset to explore"),
      selectInput('select_dataset', 'Select Dataset:', 
                  c("Story Classification" = "stry_clss",
                    "Upper Triangle" = "uppr_trngl",
                    "Lower Triangle" = "lwr_trngl")),
      hr(),
      conditionalPanel("input.algorithms == 'BiClustering'",
                       selectInput('select_method', 'Select BiClustering Method:',
                                   c("BiMax" = "bimax", "BCCC" = "bccc")),
                       conditionalPanel(
                         condition = "input.select_method == 'bimax'",

                         numericInput("n_biclstrs", "Number of Biclusters",
                                       value = 4, 
                                       min = 1, 
                                       max = 20, 
                                       step = 1),
                          br(),
                          numericInput("minr", "Minimum number of rows",
                                       value = 4, 
                                       min = 2, 
                                       max = 100, 
                                       step = 1),
                          br(),
                          numericInput("mincol", 
                                       "Minimum number of Columns",
                                       value = 4, 
                                       min = 2, 
                                       max = 50, 
                                       step = 1)),
                       conditionalPanel(
                         condition = "input.select_method == 'bccc'",
                         numericInput("n_biclstrs", "Number of Biclusters",
                                      value = 4, 
                                      min = 1, 
                                      max = 20, 
                                      step = 1),
                         br(),
                         numericInput("alpha", "Alpha",
                                      value = 1.5, 
                                      min = 1.0, 
                                      max = 5.0, 
                                      step = 0.5),
                         br(),
                         numericInput("delta", 
                                      "Delta",
                                      value = 1.0, 
                                      min = 1.0, 
                                      max = 5.0, 
                                      step = 0.5)
                       )
                       )
    ),
    
    mainPanel(
      # fluidRow(
      #   column(3, DT::dataTableOutput('table', width = 500)),
      # ),
      tabsetPanel(id = "algorithms",
        tabPanel("t-SNE", 
                 sliderInput("n_iters", "Number of iterations", 
                             min = 500, 
                             max = 5000, 
                             value = 500), 
                 sliderInput("perplexity", "Perplexity", 
                             min = 5, 
                             max = 30, 
                             value = 5, 
                             step = 2),
                 hr(),
                 plotlyOutput("tsnePlot")
        ),
        tabPanel("BiClustering", 
                 headerPanel("Welcome to Biclustering"),
                 
                 tabsetPanel(id = "tabs",
                   tabPanel("Summary",
                            fluidRow(
                              column(6, style = "width: 800px;", verbatimTextOutput("biclust_summ"))
                            ),
                            # fluidRow(
                            #   InteractiveComplexHeatmapOutput("ht")
                            # )
                            # DT::dataTableOutput(outputId = "data_table", 
                            #                     width = "800px", 
                            #                     height = "900px"),
                            ),
                   tabPanel("Heatmap",
                           hr(),
                           conditionalPanel("input.select_method == 'bimax'",
                                            InteractiveComplexHeatmapOutput(heatmap_id = "ht",
                                                                            title1 = "All BiClusters", 
                                                                            title2 = "Selected sub-bicluster", 
                                                                            width1 = 650, 
                                                                            height1 = 450, 
                                                                            width2 = 500),
                           ),
                           conditionalPanel(
                             condition = "input.select_method == 'bccc'",
                             plotlyOutput(outputId = "heat_map",
                                          width = "1000px",
                                          height = "900px")),
                           hr(),
                   ),
                   tabPanel("Other",
                            hr(),
                            fluidRow(
                            plotlyOutput("bic.scatter", 
                                         width = "500px", 
                                         height = "500px")),
                            fluidRow(
                            verbatimTextOutput("biclust_select_heatmap")),
                   ),
                 ),
                 
                 
                 ),
                 
                 # hr(), 
                 # br(), hr(),
                 # plotOutput("bic.plot", width = "800px", height = "900px", hover = "values")
                 
                 #          # hr(),
                 #          # plotOutput("biclustplot", width = "800px", height = "900px", hover = "values"),
        ),
      ),
    )
  )
# )

