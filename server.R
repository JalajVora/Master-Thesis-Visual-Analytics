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
library(dplyr)
# library(DT)
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
      d <- read.csv("~/Master Thesis/Story_Classification_Data.csv")
      
      return(d)
    }
  })

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
           # 'bccc' = {
           #   print(typeof(input$alpha))
           #   print(typeof(input$delta))
           #   print(typeof(input$n_biclstrs))
           #   bic.res <- biclust(x = readData(),
           #                      method=BCCC(),
           #                      delta=1.5,
           #                      alpha=1,
           #                      number=6)
           # },
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
      
      raw_data <- readrawData()
      theme_colors <- raw_data[,39]
      hover_info <- raw_data[,36]
      # print(as.integer(row.names(raw_data)))
      # print(c(1:dim(raw_data)[1]))
      
      p = plot_ly(source = "tsne_plot",
                  run.tsne(), 
                  x = ~tsne_x, 
                  y = ~tsne_y, 
                  type = 'scatter', 
                  mode = 'markers', 
                  width = "800px", 
                  height = "800px",
                  color = theme_colors,
                  text = paste0(hover_info),
                  hovertemplate = "%{text}",
                  customdata = c(1:dim(raw_data)[1])
                  ) %>%
        layout(
          selected = list(mode = "lasso"),
          
          # Remove other selection modes
          hovermode = "closest",
          dragmode = FALSE,
          selectdirection = "none",
          xaxis = list(title = "t-SNE Dimension 1"),
          yaxis = list(title = "t-SNE Dimension 2")
        )
    }
    
    else {
      p = plot_ly(source = "tsne_plot",
        run.tsne(), 
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
    
    return(p)
  })

  
  observeEvent(
    input$select_method == 'bimax',
    {
      d = readData()
      bc = run.biclust()
      
      # row_labels <- readrawData()
      # row.names(d) <- row_labels[,36]
      
      
      ones_counts <- colSums(d == 1)
      col_ha = HeatmapAnnotation(Count = anno_barplot(ones_counts))
      
      ht = Heatmap(d, 
                   cluster_rows = FALSE, 
                   top_annotation = col_ha)
      ht = draw(ht)
      
      makeInteractiveComplexHeatmap(input, output, session, ht, "ht")
    }
  )
  
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
      config(modeBarButtonsToRemove = c('zoom','zoomin', 
                                        'zoomOut', 'pan2d', 'zoomOut2d')) %>%
      layout(xaxis = list(title = "Row Count"),
             yaxis = list(title = "Column Count"),
             title = "Bicluster Row and Column Counts",
             hovermode = "closest")
    
    return(s)
  })
  
  output$bc.indiv.htmap <- renderPlot({
    
      bc.res <- run.biclust()
      data <- readData()
      
      clickData <- event_data(event = "plotly_click",
                              source = "scatter_plot")
      print(typeof(clickData$pointNumber))
      plt.number <- as.numeric(clickData$pointNumber+1)
      print(plt.number)
      
      if (is.null(clickData)){
        plt.BC.heatmap <- heatmapBC(x = data,
                                    bicResult = bc.res,
                                    number = input$n_biclstrs,
                                    local = FALSE,
                                    order = TRUE,
                                    outside = TRUE)
      } else {
        plt.BC.heatmap <- drawHeatmap(x = data,
                                    bicResult = bc.res,
                                    number = plt.number,
                                    local = TRUE)}
        
      
      return(plt.BC.heatmap)
      
      },width = 900,height = 800)
  
  output$tsneplot.summary <- renderPrint({
    clickData <- event_data(event = "plotly_selected",
                            source = "tsne_plot")
    
    tsne_output <- run.tsne()
    data = readrawData()
    
    sub_set = data[clickData$customdata,]
    
    return(sub_set$Title)
  })
  
  # output$tsne.boxplot <- renderPlotly({
  # 
  #   clickData <- event_data(event = "plotly_selected",
  #                           source = "tsne_plot")
  # 
  #   tsne_output <- run.tsne()
  #   raw_data = readrawData()
  #   cleaned_data = readData()
  #   sub_set = cleaned_data[clickData$customdata,]
  #   sub_set = as.data.frame(sub_set)
  # 
  #   
  #   count.subset.cols = sub_set %>% summarise_all(sum)
  #   percentage.subset.cols <- (count.subset.cols/dim(sub_set)[1])*100
  #   merged.subset.df = rbind(count.subset.cols, percentage.subset.cols)
  # 
  # })
  
  output$tsne.lineplot <- renderPlotly({
    
    clickData <- event_data(event = "plotly_selected",
                            source = "tsne_plot")
    
    cleaned_data = as.data.frame(readData())
    # raw_data = readrawData()
    subset_data = as.data.frame(cleaned_data[clickData$customdata,])

    count.subset.cols = subset_data %>% summarise_all(sum)
    percentage.subset.cols <- (count.subset.cols/dim(subset_data)[1])*100
    merged.subset.df = rbind(count.subset.cols, percentage.subset.cols)
    
    count.cleaned.cols = cleaned_data %>% summarise_all(sum)
    percentage.cleaned.cols <- (count.cleaned.cols/dim(cleaned_data)[1])*100
    merged.cleaned.df = rbind(count.cleaned.cols, percentage.cleaned.cols)
    
    mylines <- list()
    subset_lines <- list()
    
    for (col in c(1:length(merged.subset.df))) {
      line = list(type = "line",
                  line = list(color = "black", dash = "dot"),
                  xref = "x",
                  yref = "y",
                  x0 = as.double(col)-0.25,
                  x1 = as.double(col)+0.25,
                  y0 = merged.cleaned.df[2,col],
                  y1 = merged.cleaned.df[2,col])
      mylines <- c(mylines, list(line))
      
      cl_line = list(type = "line",
                  line = list(color = "red"),
                  xref = "x",
                  yref = "y",
                  x0 = as.double(col)-0.25,
                  x1 = as.double(col)+0.25,
                  y0 = merged.subset.df[2,col],
                  y1 = merged.subset.df[2,col])
      mylines <- c(mylines, list(cl_line))
      
      myrect = list(type = "rect",
                  line = list(color = "red"),
                  opacity = 0.3,
                  fillcolor = "red",
                  xref = "x",
                  yref = "y",
                  x0 = as.double(col)-0.25,
                  x1 = as.double(col)+0.25,
                  y0 = merged.cleaned.df[2,col],
                  y1 = merged.subset.df[2,col])
      mylines <- c(mylines, list(myrect))
    }
    
    fig <- plot_ly()
    
    fig <- layout(fig, title = 'Highlighting with Lines', shapes = mylines)
    
    return(fig)
    
  })
}