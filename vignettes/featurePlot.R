## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----eval=FALSE---------------------------------------------------------------
# library(scVisual)
# 
# # Load Seurat object (example)
# # seurat_obj <- readRDS(system.file("extdata", "seuratTest.RDS", package = "scVisual"))
# 
# # Create feature plot
# # featurePlot(seurat_obj, features = c("CD3E", "CD14"))


## ----eval=FALSE---------------------------------------------------------------
# # Create customized feature plot
# # featurePlot(
# #   seurat_obj,
# #   features = c("CD3E", "CD14"),
# #   cols = c("white", "red"),
# #   reduction = "tsne",
# #   split.by = "orig.ident"
# # )


## ----eval=FALSE---------------------------------------------------------------
# # Plot multiple features
# # featurePlot(
# #   seurat_obj,
# #   features = c("CD3E", "CD14", "MS4A1", "CD8A"),
# #   cols = c("lightgrey", "darkblue"),
# #   ncol = 2
# # )


## ----eval=FALSE---------------------------------------------------------------
# # Create feature plot and customize
# # p <- featurePlot(seurat_obj, features = "CD3E")
# # p +
# #   theme_bw() +
# #   labs(title = "CD3E Expression")

