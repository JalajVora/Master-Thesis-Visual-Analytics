#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(Rtsne)
library(biclust)
library(plotly)
library(curl)

# pat_token <- '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'
url = "https://raw.githubusercontent.com/JalajVora/Master-Thesis-Visual-Analytics/main/data/Story_Classification_Data.csv"

# Define server logic
function(input, output, session) {
  
  readData <- reactive({
    
    if (input$select_dataset == "stry_clss") {
      d <- read.csv(curl(url))
      d <- d[,1:35]
      d <- d[, sapply(d, is.numeric)]
      d[is.na(d)] <- 0
      d <- as.matrix(d)
      
      return(d)
    }
    
    if (input$select_dataset == "uppr_trngl") {
      mat <- matrix(1, 100, 100)
      mat[lower.tri(mat)] <- 0
      d <- as.matrix(mat)
      
      return(d)
    }
    
    if (input$select_dataset == "lwr_trngl") {
      mat <- matrix(1, 100, 100)
      mat[upper.tri(mat)] <- 0
      d <- as.matrix(mat)
      
      return(d)
    }
    
  })
  
  readrawData <- reactive({
    
    if (input$select_dataset == "stry_clss") {
      d <- read.csv(url)
      
      return(d)
    }
  })
  
  prep_data <- reactive({
    if (input$select_dataset == "stry_clss") {
      d <- readData()
      d <- d[,1:35]
      d <- d[, sapply(d, is.numeric)]
      d[is.na(d)] <- 0
      d <- as.matrix(d)
    }
    
    return(d)
  })
  
  run.tsne <- reactive({
    
    #input parameters to run t-SNE
    tsne_output <- Rtsne(readData(), check_duplicates= FALSE, 
                         max_iter = input$n_iters, 
                         perplexity=input$perplexity)
    tsne_output_df <- as.data.frame(tsne_output$Y)
    
    #preparing t-SNE output to visualise
    colnames(tsne_output_df) <- c("tsne_x", "tsne_y")
    
    return(tsne_output_df)
  })
  
  run.biclust <- reactive({
    
    # using the biclust function to perform biclustering
    bic.res <- biclust(x = readData(), 
                       method=BCBimax(), 
                       minr=as.numeric(input$minr), 
                       minc=as.numeric(input$mincol), 
                       number=as.numeric(input$n_biclstrs)
    )
    print("In here")
    return(bic.res)
  })
  
  output$tsnePlot <- renderPlotly({
    
    if (input$select_dataset == "stry_clss") {
      
      row_names <- readrawData()
      row_names <- row_names[,39]
      
      plot_ly(run.tsne(), 
              x = ~tsne_x, 
              y = ~tsne_y, 
              type = 'scatter', 
              mode = 'markers', 
              width = "800px", 
              height = "800px",
              color = row_names
      ) %>%
        layout(xaxis = list(title = "t-SNE Dimension 1"),
               yaxis = list(title = "t-SNE Dimension 2")
        )
    }
    
    else {
      plot_ly(run.tsne(), 
              x = ~tsne_x, 
              y = ~tsne_y, 
              type = 'scatter', 
              mode = 'markers', 
              width = "800px", 
              height = "800px" 
      ) %>%
        layout(xaxis = list(title = "t-SNE Dimension 1"),
               yaxis = list(title = "t-SNE Dimension 2")
        )
    }
  })
  
  output$bic.plot <- renderPlot(
    heatmapBC(x = readData(), bicResult = run.biclust(), outside = TRUE)
  )
  
  output$heat_map <- renderPlotly({
    d = readData()
    bc = biclust(d, 6, method = BCCC())
    print("before plotly")
    p = plot_ly(z = d[bc@RowxNumber, bc@NumberxCol], 
                x = colnames(d)[bc@RowxNumber], 
                y = rownames(d)[bc@NumberxCol],
                colorscale = "Viridis",
                type = "heatmap")
    print("final")
    return(p)
  })
  
  # output$bic.scatter <- renderPlotly({
  #   
  #   clusters <- bicluster(x = )
  #   
  #   # Count number of rows and columns in each bicluster
  #   rows <- sapply(bc@RowxNumber, length)
  #   cols <- sapply(bc@NumberxCol, length)
  #   # Create scatter plot with counts as x and y coordinates
  #   plot_ly(x = rows, y = cols, mode = "markers",
  #           # marker = list(size = 10),
  #           type = "scatter",
  #           hoverinfo = "text", 
  #           text = paste("Bicluster ID: ", 1:length(rows)))
  # })
  
  # output$biclustplot <- renderPlotly({
  #   if (input$select_dataset == "stry_clss") {
  #     xmat <- prep_data()
  #   }
  #   
  #   else {
  #     xmat <- readData()
  #   }
  #   
  #   bc <- run.biclust()
  #   
  #   plot_ly(z = xmat[bc@RowxNumber, bc@NumberxCol], 
  #           x = colnames(xmat)[bc@NumberxCol], 
  #           y = rownames(xmat)[bc@RowxNumber],
  #           colorscale = "Viridis",
  #           type = "heatmap")
  #   # heatmapBC(x=xmat, bicResult = run.biclust(), outside = TRUE)
  # })
}
