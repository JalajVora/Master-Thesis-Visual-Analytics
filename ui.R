library(shiny)
library(plotly)
library(shinythemes)
library(ComplexHeatmap)
library(InteractiveComplexHeatmap)

fluidPage(theme = shinytheme(theme = "united"),
  
  # headerPanel("Semi-automated pattern detection in tabular data with weak hierarchies"),
  navbarPage("Visual Analytics:",
             tabPanel("Home",
  
  sidebarLayout(
    sidebarPanel(
      tags$h3("Input:"),
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
                                       value = 5, 
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
                 fluidRow(
                   column(6, plotlyOutput("tsneScatterPlot")
                          ),br(), br(), br(),
                   # column(3, style = "width: 400px;", verbatimTextOutput("tsneplot.summary") ), br(),
                   ),br(), br(), br(),
                 hr(),
                 fluidRow(
                   column(6, style = "width: 900px;", plotlyOutput("tsneUsageDeviationPlot")
                   ),br(), br(), br(),
                 ),
                 hr(),
                 fluidRow(
                   column(6, style = "width: 900px;", plotlyOutput("tsneSimilarityPlot")
                   ),br(), br(), br(),
                 ),
                 # fluidRow(
                 #   column(6, style = "width: 900px;", plotlyOutput("tsneSamplePlot")
                 #   ),br(), br(), br(),
                 #   # column(6, style = "width: 400px;", verbatimTextOutput("tsneSamplePrint")
                 #   # ),br(), br(), br(),
                 # )
                 ),
                 
        tabPanel("BiClustering", 
                 headerPanel("Welcome to Biclustering"),
                 
                 tabsetPanel(id = "tabs",
                   tabPanel("Summary",
                            fluidRow(
                              column(6, style = "width: 800px;", verbatimTextOutput("biclust_summ"))
                              ), hr(), br(), br(), br(), br(),
                            fluidRow(
                              column(6, InteractiveComplexHeatmapOutput(heatmap_id = "ht",
                                                                        title1 = "Story Classification Data",
                                                                        title2 = "Selected subset",
                                                                        width1 = 750,
                                                                        height1 = 700,
                                                                        width2 = 500))
                            ),
                            ),
                   # tabPanel("Heatmap",
                   #         hr(),
                   #         conditionalPanel("input.select_method == 'bimax'",
                   #                          InteractiveComplexHeatmapOutput(heatmap_id = "ht",
                   #                                                          title1 = "All BiClusters",
                   #                                                          title2 = "Selected bicluster",
                   #                                                          width1 = 650,
                   #                                                          height1 = 450,
                   #                                                          width2 = 500)
                   #                          ),
                   #         ),
                   tabPanel("Bicluster Exploration",
                            h4("Explore Biclusters found"),
                            hr(),
                            fluidRow(
                              column(6, plotlyOutput("bic.scatter",
                                                     width = "100%",
                                                     height = "60%")
                                     ),
                              column(6, plotlyOutput("bc.indiv.htmap",
                                                     width = "100%",
                                                     height = "100%"),
                                     # offset = 1,
                                     ),
                              ),
                            hr(),
                            fluidRow(
                              column(12, plotlyOutput("biclusterSimilarityDendogramPlot",
                                                     width = '100%',
                                                     height = '100%')),
                              # column(4, plotlyOutput("biclusterSimilarityBoxPlot",
                              #                        width = "600px",
                              #                        height = "500px"))
                            ),
                            # fluidRow(
                            #   
                            # ),
                   ),
                 ),
              ),
      ),
      width=8,),
        ),
             ),
  tabPanel("About"),
             
  )
)
