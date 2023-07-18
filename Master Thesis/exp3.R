bc = biclust(maize, 6, method = BCCC())
heatmap = plot_ly(z = maize[bc$row_ind, bc$col_ind], 
                  x = colnames(maize)[bc$col_ind], 
                  y = rownames(maize)[bc$row_ind],
                  colorscale = "Viridis",
                  type = "heatmap")
heatmap