library(shiny)
library(plotly)
library(biclust)
library(dplyr)

# # Load example data
# # loading data into a matrix
data <- read.csv("/Documents and Settings/Jalaj/Documents/Master Thesis/Story_Classification_Data.csv")

#pre-processing data for bi-clustering
matrix <- data[,1:35]
matrix[is.na(matrix)] <- 0
matrix <- as.matrix(matrix)

data1 <- matrix(sample(c(0,1),100*100,replace=TRUE,prob=c(0.9,0.1)),nrow=100,ncol=100)
data1[1:10,1:10] <- 1 # BC1
data1[11:20,11:20] <- 1 # BC2
data1[21:30,21:30] <- 1 # BC3
data1 <- as.matrix(data1[sample(1:nrow(data1),nrow(data1)),sample(1:ncol(data1),ncol(data1))])
maize <- data1

# Find biclusters using BiG package
bc <- biclust(matrix, method = BCBimax(), number = 3, minr = 2, minc = 2)

heatmapBC(matrix, bicResult = bc, outside = TRUE)
# Define UI
ui <- fluidPage(
  
  summary(bc),
  hr(),
  # Create scatter plot
  plotlyOutput("scatter", width = "800px"),
  
  # Create heatmap
  # plotlyOutput("heatmap")
)

# Define server
server <- function(input, output) {
  
  # Create scatter plot
  output$scatter <- renderPlotly({
    # Count number of rows and columns in each bicluster
    rows <- sapply(bc@RowxNumber, length)
    cols <- sapply(bc@NumberxCol, length)
    # Create scatter plot with counts as x and y coordinates
    plot_ly(source = "scatter_map",
            x = rows, y = cols, mode = "markers",
            marker = list(size = 10),
            type = "scatter",
            hoverinfo = "text", 
            text = paste("Bicluster ID: ", 1:length(rows)))%>%
      event_register("plotly_click")
  })
  
  #Create heatmap for selected bicluster
  output$heatmap <- renderPlotly({
    # Get selected bicluster from scatter plot
    # event_register(p, 'plotly_click', scatter) %>%
    event_data("plotly_click", source = "scatter_map") %>%
      if (!is.null(.)) {
        pull(pointNumber) %>%
          # Create heatmap for selected bicluster
          plot_ly(z = maize[bc@RowxNumber[[.]], bc@NumberxCol[[.]]],
                  colorscale = "Viridis", type = "heatmap")
      }

  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
