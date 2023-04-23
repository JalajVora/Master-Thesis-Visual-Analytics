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
library(ggplot2)
library(ggdendro)
library(curl)
library(dplyr)
library(superbiclust)
library(fabia)
library(ComplexHeatmap)
library(InteractiveComplexHeatmap)
library(colormod)

url = "https://raw.githubusercontent.com/JalajVora/Master-Thesis-Visual-Analytics/main/data/Story_Classification_Data.csv"
options (warn = -1)
tsne_similarity_matrix = matrix()

getCurrentTime = function() {
  now = Sys.time()
  return (as.numeric(now))
}

printTime = function(time) {
  print(time, digits=20)
}

number2binary = function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  if(missing(noBits)) {
    return(binary_vector)
  } else {
    binary_vector = binary_vector[-(1:(length(binary_vector) - noBits))]
    return(binary_vector)
  }
}

createUniqueColors = function(num_colors) {
  unique_colors = matrix(data=0, nrow=3, ncol=num_colors)
  for (i in 0:(num_colors-1)) {
    h = i * (1.0 / num_colors)
    hsvcolor = matrix(data=list(h,1,1), nrow=3, ncol=1, dimnames=list(c('h','s','v')))
    rgbcolor = hsv2rgb(hsvcolor)
    unique_colors[,i+1] = rgbcolor
  }
  return(unique_colors)
}

createCustomColorPallete = function(unique_colors) {
  num_colors = dim(unique_colors)[2]
  all_colors = list()
  all_colors = append(all_colors, "#FFFFFF80")
  for (i in 1:num_colors) {
    all_colors = append(all_colors, rgb(unique_colors[1,i], unique_colors[2,i], unique_colors[3,i], 128, maxColorValue = 255))
  }
  all_colors = append(all_colors, "#00000080")
  return(as.character(all_colors))
}

createBinaryEncodedMatrix = function(matrix_3d) {
  num_rows = dim(matrix_3d)[1]
  num_cols = dim(matrix_3d)[2]
  num_bicluster = dim(matrix_3d)[3]
  a = array(data=0, dim=c(num_rows, num_cols))
  for (i in 1:num_rows) {
    for (j in 1:num_cols) {
      for (k in 1:num_bicluster) {
        if (matrix_3d[i,j,k] == 1){
          if (a[i,j] == 0){
            a[i,j] = k
          }
          else{
            a[i,j] = num_bicluster+1
          }
        }
      }
    }
  }
  return(a)
}

createSimilarityMatrix = function(m) { # input matrix m has booleans
  num_rows = dim(m)[1]
  num_cols = dim(m)[2]
  tsne_similarity_matrix <<- matrix(data=NA, nrow=num_rows, ncol=num_rows)
  for (i in 1:num_rows) {
    for (j in i:num_rows) {
      if (i==j) {
        next
      }
      xor_row = xor(m[i,],m[j,])
      similarity_count = num_cols - sum(xor_row)
      tsne_similarity_matrix[i,j] <<- as.double(similarity_count)/num_cols
    }
  }
}

# Define server logic
function(input, output, session) {
  
  onStop(function() cat("This will run on session stop\n"))
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

    return(bic.res)
  })
  
  output$biclust_summ <- renderPrint({
    bic.res <- run.biclust()
    return(summary(bic.res))
  })
  
  output$tsneScatterPlot <- renderPlotly({
    
    if (input$select_dataset == "stry_clss") {
      
      raw_data <- readrawData()
      theme_colors <- raw_data[,39]
      hover_info <- raw_data[,36]
      
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
      createSimilarityMatrix(readData())
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
    event_register(p = p, event = "plotly_selected")
    return(p)
  })
  
  output$tsneUsageDeviationPlot <- renderPlotly({
    
    clickData <- event_data(event = "plotly_selected",
                            source = "tsne_plot")
    
    if (is.null(clickData)) return(NULL)
    
    cleaned_data = as.data.frame(readData())
    subset_data = as.data.frame(cleaned_data[clickData$customdata,])
    cleaned_data_cols <- colnames(cleaned_data)
    
    
    fig <- plotly_empty(x = cleaned_data,
                        type="bar",
                        mode = "markers",
                        marker = list(color = "rgba(0,0,0,0)"))
    
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
      line_color = "green"
      if (merged.subset.df[2, col]<merged.cleaned.df[2, col]) {
        line_color = "red"
      }
      
      cl_line = list(type = "line",
                     line = list(color = line_color),
                     xref = "x",
                     yref = "y",
                     x0 = as.double(col)-0.25,
                     x1 = as.double(col)+0.25,
                     y0 = merged.subset.df[2,col],
                     y1 = merged.subset.df[2,col])
      mylines <- c(mylines, list(cl_line))
      
      myrect = list(type = "rect",
                    line = list(color = line_color),
                    opacity = 0.3,
                    fillcolor = line_color,
                    xref = "x",
                    yref = "y",
                    x0 = as.double(col)-0.25,
                    x1 = as.double(col)+0.25,
                    y0 = merged.cleaned.df[2,col],
                    y1 = merged.subset.df[2,col])
      mylines <- c(mylines, list(myrect))
    }
    
    fig <- layout(fig,
                  title = "Usage Deviation Plot",
                  showlegend = FALSE,
                  autosize = TRUE,
                  xaxis = list(title = "Storytelling Techniques",
                               autotypenumbers = 'strict'),
                  yaxis = list(title = "Usage %",
                               showgrid = TRUE),
                  shapes = mylines)
    
    fig <- fig %>% layout(xaxis = list(
        tickmode = "array",
        tickvals = 1:length(cleaned_data_cols),
        ticktext = cleaned_data_cols,
        showgrid = TRUE))
    
    return(fig)
    
  })
  
  
  output$tsneSimilarityPlot <- renderPlotly({
    clickData <- event_data(event = "plotly_selected",
                            source = "tsne_plot")
    
    if (is.null(clickData)) return(NULL)
    
    raw_data <- readrawData()
    # theme_colors <- raw_data[,39]
    cleaned_data = as.data.frame(readData())
    # cleaned_data = as.data.frame(raw_data)
    subset_data = as.data.frame(cleaned_data[clickData$customdata,])
    fig <- plotly_empty(type="scatter", mode = "markers")

    dim_c_2 = (dim(cleaned_data)[1] * (dim(cleaned_data)[1]-1))/2
    total_similarity = sum(tsne_similarity_matrix, na.rm = TRUE)/dim_c_2
    
    subset_similarity = double(dim(subset_data)[1])
    for (i in 1:dim(subset_data)[1]) {
      sum_similarity = sum(tsne_similarity_matrix[i,], na.rm=TRUE)
                     + sum(tsne_similarity_matrix[,i], na.rm=TRUE)
      subset_similarity[i] = sum_similarity/(dim(cleaned_data)[1]-1)
    }
    subset_similarity = subset_similarity*100
    
    fig = plot_ly(source = "scatter_plot",
                  x = subset_similarity, 
                  y = integer(dim(subset_data)[1]),
                  type = "scatter", 
                  mode = "markers",
                  # color = theme_colors,
                  marker = list(size = 10),
                  showlegend = F,
                  hoverinfo = 'text',
                  text = paste0(round(subset_similarity,1), " %"))
    
    
    mylines <- list()
    line = list(type = "line",
                line = list(color = "red", dash = "solid"),
                xref = "x",
                yref = "y",
                x0 = total_similarity*100,
                x1 = total_similarity*100,
                y0 = 0,
                y1 = 1)
    mylines <- c(mylines, list(line))
    
    fig <- layout(fig,
                  title = "Deviation of subset from overall average",
                  xaxis = list(title = "Pairwise Similarity %",
                               zeroline = TRUE,
                               autotypenumbers = 'strict'),
                  yaxis = list(showticklabels = FALSE),
                  shapes = mylines)
    return(fig)
  })

  
  observeEvent(
    input$select_method == 'bimax',
    {
      d = readData()
      rownames(d) = paste0("Story ", 1:130)
      # bc = run.biclust()
      
      # row_labels <- readrawData()
      # row.names(d) <- row_labels[,36]

      # print(bc@RowxNumber)
      # print(bc@NumberxCol)

      ones_counts <- colSums(d == 1)
      col_ha = HeatmapAnnotation(Count = anno_barplot(ones_counts))

      ht = Heatmap(d,
                   cluster_rows = FALSE,
                   top_annotation = col_ha)
      ht = draw(ht)
      #
      makeInteractiveComplexHeatmap(input, output, session, ht, "ht")
      
    }
  )
  
  output$bic.scatter <- renderPlotly({

    bic.res <- run.biclust()
    num_of_bic <- bic.res@Number
    
    bic.rowxcol <- data.frame(matrix(nrow = num_of_bic, ncol=2))
    colnames(bic.rowxcol) <- c("row_count", "col_count")
    unique_colors = createUniqueColors(num_of_bic)
    my_color_pallete = createCustomColorPallete(unique_colors)
    print("Scatter colors before indexing: ")
    print(my_color_pallete)
    my_color_pallete = my_color_pallete[-1][-(num_of_bic+1)]
    print("Scatter colors after indexing: ")
    print(my_color_pallete)
    
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
                 color = as.numeric(row.names(bic.rowxcol)),
                 colors = my_color_pallete,
                 marker = list(size = 10),
                 showlegend = F,
                 customdata = c(1:length(bic.rowxcol$row_count))
                 ) %>% 
      config(modeBarButtonsToRemove = c('zoom','zoomin', 
                                        'zoomOut', 'pan2d', 'zoomOut2d')) %>%
      layout(xaxis = list(title = "Row Count"),
             yaxis = list(title = "Column Count"),
             title = "Bicluster Row and Column Counts",
             hovermode = "closest")
    
    return(s)
  })
  
  output$bc.indiv.htmap <- renderPlotly({
    
    clickData <- event_data(event = "plotly_click",
                            source = "scatter_plot",
                            priority = c("input"))
    start_time = getCurrentTime()
    data <- readData()
    rawData <- readrawData()
    bc.res <- run.biclust()
    after_biclust_time = getCurrentTime()
    print(clickData)
    plt.number <- as.numeric(clickData$customdata)
    print(plt.number)
    if (!is.null(clickData) && (length(plt.number)>0) && (plt.number > bc.res@Number)) {
      clickData <- NULL
    }
    # print(clickData$customdata)
    # subset_data = as.data.frame(cleaned_data[clickData$customdata,])
    
    num_rows = dim(data)[1]
    num_cols = dim(data)[2]
    # num_bicluster = dim(bc.res@RowxNumber)[2]
    num_bicluster = bc.res@Number
    
    cluster_array = array(dim=c(num_rows, num_cols, num_bicluster))
    for (i in 1:num_rows) {
      for (j in 1:num_cols) {
        cluster_array[i,j,] = bc.res@RowxNumber[i,] & bc.res@NumberxCol[,j]
      }
    }
    after_cluster_array_init_time = getCurrentTime()
    
    bin_enc_mat = createBinaryEncodedMatrix(cluster_array)
    after_bin_enc_time = getCurrentTime()
    
    unique_colors = createUniqueColors(num_bicluster)
    after_unique_colors_time = getCurrentTime()
    
    my_color_pallete = createCustomColorPallete(unique_colors)
    print(my_color_pallete)
    print(plt.number)
    after_my_color_pallete_time = getCurrentTime()
    
    rownames(data) = paste0("Story ", 1:130)
    story_titles = rawData[,36]
    
    if (is.null(clickData)){
      plt.BC.heatmap = plot_ly(z=bin_enc_mat,
                               x = colnames(data),
                               y = rownames(data),
                               colors = my_color_pallete,
                               type = "heatmap",
                               opacity = 0.5,
                               showlegend = T,
                               showscale = FALSE,
                               hoverinfo = 'none')
    } else {
      plt.BC.heatmap = plot_ly(z= 1 * cluster_array[,,plt.number],
                               x = colnames(data),
                               y = rownames(data),
                               colors = c("white",
                                          my_color_pallete[plt.number+1]),
                               type = "heatmap",
                               opacity = 0.5,
                               showlegend = T,
                               showscale = FALSE,
                               hoverinfo = 'none')
    }
    
    plt.BC.heatmap <- plt.BC.heatmap %>% 
      layout(title = 'Selected Bicluster',
             xaxis = list(title = 'Storytelling Techniques'), 
             yaxis = list(title = 'Stories'))
    
    return(plt.BC.heatmap)
  })
  
  output$biclusterSimilarityDendogramPlot <- renderPlotly({
    
    cleanedData = readData()
    resBIC = run.biclust()
    numBIC = input$n_biclstrs
    
    BiMaxBiclustSet <-  BiclustSet(resBIC)
    SensitivityMatr<- similarity(BiMaxBiclustSet,index="sensitivity")
    rownames(SensitivityMatr) = colnames(SensitivityMatr) = paste0("BC ", 1:numBIC)
    # print(SensitivityMatr)
    HCLMat <- HCLtree(SensitivityMatr)
    dg = as.dendrogram(HCLMat)
    p = ggdendrogram(dg, rotate = FALSE, size = 2)
    plt = ggplotly(p, dynamicTicks = FALSE) %>%
      layout(title = 'Bicluster Ranking',
             xaxis = list(title = 'Biclusters'), 
             yaxis = list(title = 'Jaccardian Similarity Index'))
    
    return(plt)
    
  })
  
  # output$biclusterSimilarityBoxPlot <- renderPlotly({
  #   resBIC = run.biclust()
  #   numBIC = input$n_biclstrs
  #   
  #   BiMaxBiclustSet <-  BiclustSet(resBIC)
  #   simMatr<- similarity(BiMaxBiclustSet,index="jaccard", type="both")
  #   rownames(simMatr) <- paste("BC", 1:numBIC, sep = "")
  #   colnames(simMatr) <- paste("BC", 1:numBIC, sep = "")
  #   
  #   fig <- plot_ly(y=simMatr[1,], type = "box", name = "BC 1")
  #   for (i in 2:(dim(simMatr)[1])) {
  #     fig <- fig %>% add_trace(y=simMatr[i,], type = "box", name =  paste("BC", i))
  #   }
  #   fig <- fig %>% 
  #     layout(
  #       title = "Similarity of Biclusters",
  #       xaxis = list(title="Biclusters"),
  #       yaxis = list(title="Jaccardian Similarity Index")
  #     )
  #   
  #   return(fig)
  # })
  
  
}