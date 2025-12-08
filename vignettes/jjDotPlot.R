## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----eval=FALSE---------------------------------------------------------------
# library(scVisual)
# data(top3pbmc.markers)
# 
# # Load Seurat object (example)
# # seurat_obj <- readRDS(system.file("extdata", "seuratTest.RDS", package = "scVisual"))
# 
# # Create dot plot for top marker genes
# # jjDotPlot(seurat_obj, features = unique(top3pbmc.markers$gene))


## ----eval=FALSE---------------------------------------------------------------
# # Create customized dot plot
# # jjDotPlot(
# #   seurat_obj,
# #   features = unique(top3pbmc.markers$gene),
# #   cols = c("white", "red"),
# #   dot.scale = 8,
# #   group.by = "orig.ident"
# # )


## ----eval=FALSE---------------------------------------------------------------
# # Plot specific genes
# # jjDotPlot(
# #   seurat_obj,
# #   features = c("CD3E", "CD14", "MS4A1"),
# #   title = "Key Immune Cell Markers"
# # )


## ----eval=FALSE---------------------------------------------------------------
# # Create dot plot and customize
# # p <- jjDotPlot(seurat_obj, features = unique(top3pbmc.markers$gene))
# # p +
# #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
# #   labs(title = "Top Marker Genes")

