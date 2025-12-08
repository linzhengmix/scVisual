## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----eval=FALSE---------------------------------------------------------------
# library(scVisual)
# data(pbmc.markers)
# 
# # Create enhanced volcano plot
# jjVolcano(pbmc.markers, group_by = "cluster")


## ----eval=FALSE---------------------------------------------------------------
# # Create customized volcano plot
# jjVolcano(
#   pbmc.markers,
#   group_by = "cluster",
#   threshold_x = 0.5,
#   threshold_y = 0.01,
#   label_top = 5,
#   color_by = "cluster"
# )


## ----eval=FALSE---------------------------------------------------------------
# # Focus on specific clusters
# naive_cd4_markers <- pbmc.markers[pbmc.markers$cluster %in% c("Naive CD4 T", "Memory CD4 T"), ]
# jjVolcano(naive_cd4_markers, group_by = "cluster")


## ----eval=FALSE---------------------------------------------------------------
# # Create volcano plot and customize
# p <- jjVolcano(pbmc.markers, group_by = "cluster")
# p +
#   theme_minimal() +
#   labs(title = "Enhanced Volcano Plot")

