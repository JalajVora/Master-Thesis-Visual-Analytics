library(biclust)

url = "https://raw.githubusercontent.com/JalajVora/Master-Thesis-Visual-Analytics/main/data/Story_Classification_Data.csv?token=GHSAT0AAAAAAB7T7XXE7V7ZJEZCIASMJSIQZAHHBSA"
d <- read.csv(url)
d <- d[,1:35]
d <- d[, sapply(d, is.numeric)]
d[is.na(d)] <- 0
d <- as.matrix(d)

bic.res <- biclust(x = d, method = BCBimax(), minr = 4, minc = 4, number = 3)

bc = biclust(d, 6, method = BCCC(), number = 3)

heatmapBC(x = d, bicResult = bic.res)

heatmap = plot_ly(z = d[bc@RowxNumber, bc@NumberxCol], 
                  x = colnames(d)[bc@RowxNumber], 
                  y = rownames(d)[bc@NumberxCol],
                  colorscale = "Viridis",
                  type = "heatmap")

heatmap

drawHeatmap(d, bicResult = bic.res, number = 1)
