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
# # Create scatter plot of gene expression
# # scatterCellPlot(seurat_obj, x = "CD3E", y = "CD4", color_by = "seurat_clusters")


## ----eval=FALSE---------------------------------------------------------------
# # Create customized scatter plot
# # scatterCellPlot(
# #   seurat_obj,
# #   x = "CD3E",
# #   y = "CD4",
# #   color_by = "seurat_clusters",
# #   group_by = "orig.ident",
# #   alpha = 0.7,
# #   size = 2
# # )


## ----eval=FALSE---------------------------------------------------------------
# # Plot metadata relationships
# # scatterCellPlot(
# #   seurat_obj,
# #   x = "nCount_RNA",
# #   y = "nFeature_RNA",
# #   color_by = "seurat_clusters",
# #   title = "UMI Count vs. Feature Count"
# # )


## ----eval=FALSE---------------------------------------------------------------
# # Create scatter plot and customize
# # p <- scatterCellPlot(seurat_obj, x = "CD3E", y = "CD4", color_by = "seurat_clusters")
# # p +
# #   theme_minimal() +
# #   labs(subtitle = "T Cell Markers")

