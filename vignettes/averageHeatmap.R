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
# # Create average expression heatmap
# # averageHeatmap(seurat_obj, features = unique(top3pbmc.markers$gene))


## ----eval=FALSE---------------------------------------------------------------
# # Create customized heatmap
# # averageHeatmap(
# #   seurat_obj,
# #   features = unique(top3pbmc.markers$gene),
# #   group_by = "orig.ident",
# #   scale = FALSE,
# #   cluster_rows = FALSE
# # )


## ----eval=FALSE---------------------------------------------------------------
# # Plot specific genes
# # averageHeatmap(
# #   seurat_obj,
# #   features = c("CD3E", "CD14", "MS4A1", "CD8A", "FCGR3A"),
# #   title = "Key Immune Cell Markers"
# # )


## ----eval=FALSE---------------------------------------------------------------
# # Create heatmap and customize
# # p <- averageHeatmap(seurat_obj, features = unique(top3pbmc.markers$gene))
# # p + ComplexHeatmap::rowAnnotation(gene_type = "marker")

