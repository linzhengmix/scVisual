## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----eval=FALSE---------------------------------------------------------------
# library(scVisual)
# data(pbmc.markers)
# 
# # Create volcano plot for Naive CD4 T cluster
# markerVolcano(pbmc.markers, cluster = "Naive CD4 T")


## ----eval=FALSE---------------------------------------------------------------
# # Create volcano plot with custom cutoffs and title
# markerVolcano(
#   pbmc.markers,
#   cluster = "Naive CD4 T",
#   logFC_cutoff = 0.5,
#   pval_cutoff = 0.01,
#   title = "Naive CD4 T Marker Genes"
# )


## ----eval=FALSE---------------------------------------------------------------
# # Create volcano plots for multiple clusters
# markerVolcano(pbmc.markers, cluster = "Naive CD4 T")
# markerVolcano(pbmc.markers, cluster = "CD14+ Mono")


## ----eval=FALSE---------------------------------------------------------------
# # Create volcano plot and customize
# p <- markerVolcano(pbmc.markers, cluster = "Naive CD4 T")
# p +
#   theme_bw() +
#   labs(subtitle = "Differentially Expressed Genes")

