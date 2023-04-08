library(shiny)
library(plotly)
library(ComplexHeatmap)
library(InteractiveComplexHeatmap)

fluidPage(
  
  headerPanel("Semi-automated pattern detection in tabular data with weak hierarchies"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Please select a dataset to explore patterns"),
      selectInput('select_dataset', 'Select Dataset:', 
                  c("Story Classification" = "stry_clss",
                    "Upper Triangle" = "uppr_trngl",
                    "Lower Triangle" = "lwr_trngl")),
      hr(),
      conditionalPanel("input.algorithms == 't-SNE'",
                       hr(),
                       helpText("Maximum number of iterations for the optimization"),
                       sliderInput("n_iters", "Number of iterations", 
                                   min = 500, 
                                   max = 5000, 
                                   value = 500),
                       hr(),
                       helpText("Perplexity controls the balance between attention to local and global aspects of the data.\n A higher perplexity value results in more emphasis on preserving the global structure of the data, while a lower perplexity value places more emphasis on preserving the local structure of the data."),
                       sliderInput("perplexity", "Perplexity", 
                                   min = 5, 
                                   max = 30, 
                                   value = 5, 
                                   step = 2),
                       hr(),),
      conditionalPanel("input.algorithms == 'BiClustering'",
                       selectInput('select_method', 'Select BiClustering Method:',
                                   c("BiMax" = "bimax")),
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
                        ),
    width=2),
    mainPanel(
      tabsetPanel(id = "algorithms",
        tabPanel("t-SNE", 
                 headerPanel("Welcome to t-SNE"),
                 hr(),
                 hr(),
                 fluidRow(
                   column(6, style = "width: 800px;", plotlyOutput("tsnePlot")
                          ),br(), br(), br(),
                   column(6, style = "width: 800px;", verbatimTextOutput("tsneplot.summary") ), br(),
                   ),
                 hr(),
                 fluidRow(
                   column(6, style = "width: 900px;", plotlyOutput("tsne.lineplot")
                   ),br(), br(), br(),
                 )
                 ),
                 
        tabPanel("BiClustering", 
                 headerPanel("Welcome to Biclustering"),
                 
                 tabsetPanel(id = "tabs",
                   tabPanel("Summary",
                            fluidRow(
                              column(6, style = "width: 800px;", verbatimTextOutput("biclust_summ")
                                     )
                              ),
                            ),
                   tabPanel("Heatmap",
                           hr(),
                           conditionalPanel("input.select_method == 'bimax'",
                                            InteractiveComplexHeatmapOutput(heatmap_id = "ht",
                                                                            title1 = "All BiClusters",
                                                                            title2 = "Selected bicluster",
                                                                            width1 = 650,
                                                                            height1 = 450,
                                                                            width2 = 500)
                                            ),
                           ),
                   tabPanel("Scatterplot",
                            hr(),
                            fluidRow(
                              column(6, plotlyOutput("bic.scatter", width = "300px", height = "300px")
                                     ),
                              column(6, plotOutput("bc.indiv.htmap")
                                     ), br(), br(), br(),
                              ),
                            ),
                   ),
                 ),
              ),
      width=8,),
        ),
)