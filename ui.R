library(shiny)
library(plotly)
library(shinythemes)

fluidPage(theme = shinytheme(theme = "united"),
  # navbarPage(title="Pattern Detection in Tabular Data with Flat Taxonomies: A Visual Analytics Case Study",
  headerPanel('Pattern Detection in Tabular Data with shallow hierarchy: A Visual Analytics Case Study for Narrative Visualization'),
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
                       conditionalPanel("input.select_dataset == 'stry_clss'",
                       radioButtons("norm", 
                                    "Apply Hierarchichal Normalization:",
                                    c("No" = "unif", "Yes" = "norm")),),
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
      conditionalPanel("input.algorithms == 'BiClustering' && input.select_dataset == 'stry_clss'",
                       selectInput('select_method', 'Select BiClustering Method:',
                                   c("BiMax" = "bimax")),
                       conditionalPanel(
                         condition = "input.select_method == 'bimax'",

                         numericInput("n_biclstrs", "Number of Biclusters",
                                       value = 25, 
                                       min = 1, 
                                       max = 100, 
                                       step = 1),
                          br(),
                          numericInput("minr", "Minimum number of rows",
                                       value = 4, 
                                       min = 2, 
                                       max = 130, 
                                       step = 1),
                          br(),
                          numericInput("mincol", 
                                       "Minimum number of Columns",
                                       value = 4, 
                                       min = 2, 
                                       max = 35, 
                                       step = 1)),
                        ),
    width=3),
    mainPanel(
      tabsetPanel(id = "algorithms",
        tabPanel("t-SNE", 
                 headerPanel("Welcome to t-SNE"),
                 hr(),
                 fluidRow(
                   column(6, plotlyOutput("tsneScatterPlot")
                          ),br(), br(), br(),
                   ),br(), br(), br(),
                 hr(),
                 h4("Usage deviation from the average for selected subset"),
                 fluidRow(
                   column(6, style = "width: 900px;", plotlyOutput("tsneUsageDeviationPlot")
                   ),br(), br(), br(),
                 ),
                 hr(),
                 fluidRow(
                   column(6, style = "width: 900px;", plotlyOutput("tsneSimilarityPlot")
                   ),br(), br(), br(),
                 ),
                 ),
                 
          tabPanel("BiClustering", 
                   headerPanel("Welcome to Biclustering"),
                   
                   tabsetPanel(id = "tabs",
                     tabPanel("Bicluster Exploration",
                              h4("Explore Biclusters found"),
                              hr(),
                              fluidRow(
                                column(12, plotlyOutput("biclusterSimilarityDendogramPlot",
                                                        width = '100%',
                                                        height = '100%'))
                              ), hr(),
                              fluidRow(
                                column(6, plotlyOutput("bic.scatter",
                                                       width = "100%",
                                                       height = "60%")
                                       ),
                                column(6, plotlyOutput("bc.indiv.htmap",
                                                       width = "100%",
                                                       height = "100%")
                                       ),
                                ),
                     ),
                   ),
                ),
      ),
      width=8,),
        ),
             # ),
             
)
