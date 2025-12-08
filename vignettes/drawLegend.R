## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----eval=FALSE---------------------------------------------------------------
# library(scVisual)
# 
# # Create custom legend
# legend <- drawLegend(
#   colors = c("#FF0000", "#00FF00", "#0000FF"),
#   labels = c("Cluster 1", "Cluster 2", "Cluster 3"),
#   title = "Clusters",
#   orientation = "horizontal"
# )
# 
# # Display the legend
# legend


## ----eval=FALSE---------------------------------------------------------------
# library(scVisual)
# library(ggplot2)
# 
# data(pbmc.markers)
# 
# # Create a basic plot
# p <- ggplot(pbmc.markers[pbmc.markers$cluster == "Naive CD4 T", ],
#             aes(x = avg_log2FC, y = -log10(p_val_adj)))
#   geom_point()
#   labs(title = "Volcano Plot")
# 
# # Create custom legend
# legend <- drawLegend(
#   colors = c("#374E55", "#DF8F44", "#00A1D5"),
#   labels = c("Not Significant", "LogFC > 0.25", "p-adj < 0.05"),
#   title = "Significance"
# )
# 
# # Combine plot and legend (example)
# # gridExtra::grid.arrange(p, legend, ncol = 2, widths = c(4, 1))


## ----eval=FALSE---------------------------------------------------------------
# # Create highly customized legend
# drawLegend(
#   colors = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF"),
#   labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"),
#   title = "Cell Clusters",
#   orientation = "vertical",
#   size = 10,
#   font_size = 12
# )


## ----eval=FALSE---------------------------------------------------------------
# # Create and customize legend
# legend <- drawLegend(
#   colors = c("#FF0000", "#00FF00", "#0000FF"),
#   labels = c("A", "B", "C")
# )
# 
# # Further customize with ggplot2
# legend +
#   theme(legend.title = element_text(face = "bold", size = 14))

