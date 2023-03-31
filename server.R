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
library(DT)
library(ComplexHeatmap)
library(InteractiveComplexHeatmap)

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
  
  # output$data_table <- DT::renderDataTable({
  #   # DT::datatable(readrawData(), 
  #   #               width = "800px",
  #   #               height = "900px",
  #   #               autoHideNavigation = TRUE,
  #   #               style = 'bootstrap4',
  #   #               editable = FALSE)
  #   readData()
  # })
  
  run.tsne <- reactive({
    
    #input parameters to run t-SNE
    tsne_output <- Rtsne(readData(), check_duplicates= FALSE, 
                         max_iter = input$n_iters, 
                         perplexity=input$perplexity)
    
    tsne_output_df <- as.data.frame(tsne_output$Y)
    
    colnames(tsne_output_df) <- c("tsne_x", "tsne_y")
    
    
    return(tsne_output_df)
  })
  
  run.biclust <- reactive({
    switch(input$select_method, 
           'bimax' = {
             bic.res <- biclust(x = readData(),
                                method=BCBimax(),
                                minr=as.numeric(input$minr),
                                minc=as.numeric(input$mincol),
                                number=as.numeric(input$n_biclstrs))
             },
           'bccc' = {
             bic.res <- biclust(x = readData(),
                                method=BCCC(),
                                delta=as.numeric(input$delta),
                                alpha=as.numeric(input$alpha),
                                number=as.numeric(input$n_biclstrs))
           },
    )
    
    
    # bic.res <- biclust(x = readData(), 
    #                    method=BCBimax(), 
    #                    minr=4, 
    #                    minc=4, 
    #                    number=15
    # )

    return(bic.res)
  })
  
  output$biclust_summ <- renderPrint({
    bic.res <- run.biclust()
    return(summary(bic.res))
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
              height = "800px",
              showlegend = F
      ) %>%
        layout(xaxis = list(title = "t-SNE Dimension 1"),
               yaxis = list(title = "t-SNE Dimension 2")
        )
    }
  })
  
  output$heat_map <- renderPlotly({
    d = readData()
    bc = run.biclust()
    
    p = plot_ly(z = d[bc@RowxNumber, bc@NumberxCol],
                x = colnames(d)[bc@RowxNumber],
                y = rownames(d)[bc@NumberxCol],
                type = "heatmap",
                showscale = FALSE,name = "All Biclusters")

    return(p)
  })

  
  observeEvent(
    input$select_method == 'bimax',
    {
      d = readData()
      bc = run.biclust()
      ht = Heatmap(d)
      ht = draw(ht)
      
      makeInteractiveComplexHeatmap(input, output, session, ht, "ht")
    }
  )
  
  
  # output$heat_map_bimax <- renderPlotly({
  #   
  #   
  #   
  #   # rows <- attr(bc, "row")
  #   # cols <- attr(bc, "col")
  #   # subset_data <- d[rows, cols]
  #   # my_colors <- colorRampPalette(c("white", "black"))(2)
  #   
  # 
  #   
  #   # heatmaply(run.biclust())
  #             # scale_fill_gradient_fun = ggplot2::scale_fill_viridis_c, 
  #             # showticklabels = TRUE
  #             
  # })
  
  output$bic.scatter <- renderPlotly({

    bic.res <- run.biclust()
    num_of_bic <- bic.res@Number
    
    bic.rowxcol <- data.frame(matrix(nrow = num_of_bic, ncol=2))
    colnames(bic.rowxcol) <- c("row_count", "col_count")
    
    # Get the number of rows and columns in each bicluster
    for (i in 1:num_of_bic) {
      bic.rowxcol[i, 1] <- sum(bic.res@RowxNumber[,i] != 0)
      bic.rowxcol[i, 2] <- sum(bic.res@NumberxCol[i,] != 0)
    }

    s <- plot_ly(source = "scatter_plot",
                 bic.rowxcol, 
                 x = ~row_count, 
                 y = ~col_count,
                 type = "scatter", 
                 mode = "markers",
                 marker = list(size = 10),
                 showlegend = F
                 ) %>% 
      config(modeBarButtonsToRemove = c('zoom','zoomin', 'zoomOut', 'pan2d', 'zoomOut2d')) %>%
      layout(xaxis = list(title = "Row Count"),
             yaxis = list(title = "Column Count"),
             title = "Bicluster Row and Column Counts",
             hovermode = "closest")
    
    return(s)
  })
  
  output$biclust_select_heatmap <- renderPrint(
    {
      d <- event_data(event = "plotly_selected", 
                      source = "scatter_plot")
      
      bic.res <- run.biclust()
      num_of_bic <- bic.res@Number
      
      bic.rowxcol <- data.frame(matrix(nrow = num_of_bic, ncol=2))
      colnames(bic.rowxcol) <- c("row_count", "col_count")
      
      # Get the number of rows and columns in each bicluster
      for (i in 1:num_of_bic) {
        bic.rowxcol[i, 1] <- sum(bic.res@RowxNumber[,i] != 0)
        bic.rowxcol[i, 2] <- sum(bic.res@NumberxCol[i,] != 0)
      }
      
      
    }
  )
}
