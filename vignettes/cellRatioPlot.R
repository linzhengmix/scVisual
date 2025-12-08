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
# # Create cell ratio plot
# # cellRatioPlot(seurat_obj, group_by = "orig.ident")


## ----eval=FALSE---------------------------------------------------------------
# # Create customized cell ratio plot
# # cellRatioPlot(
# #   seurat_obj,
# #   group_by = "orig.ident",
# #   split_by = "seurat_clusters",
# #   normalize = FALSE
# # )


## ----eval=FALSE---------------------------------------------------------------
# # Compare cell ratios across conditions
# # cellRatioPlot(
# #   seurat_obj,
# #   group_by = "condition",
# #   split_by = "seurat_clusters",
# #   title = "Cell Type Distribution Across Conditions"
# # )


## ----eval=FALSE---------------------------------------------------------------
# # Create cell ratio plot and customize
# # p <- cellRatioPlot(seurat_obj, group_by = "orig.ident")
# # p +
# #   theme_bw() +
# #   labs(subtitle = "Cell Distribution") +
# #   theme(axis.text.x = element_text(angle = 45, hjust = 1))

