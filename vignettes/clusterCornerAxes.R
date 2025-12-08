## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----eval=FALSE---------------------------------------------------------------
# library(scVisual)
# data(pbmc.markers)
# 
# # Prepare cluster-level data
# top_genes <- top3pbmc.markers$gene
# summary_data <- aggregate(
#   avg_log2FC ~ cluster,
#   data = pbmc.markers[pbmc.markers$gene %in% top_genes, ],
#   FUN = mean
# )
# 
# # Create cluster corner axes plot
# clusterCornerAxes(summary_data, cluster_variable = "cluster", features = "avg_log2FC")


## ----eval=FALSE---------------------------------------------------------------
# # Create customized cluster corner axes plot
# clusterCornerAxes(
#   summary_data,
#   cluster_variable = "cluster",
#   features = "avg_log2FC",
#   color_by = "cluster"
# )


## ----eval=FALSE---------------------------------------------------------------
# # Create cluster corner axes plot and customize
# p <- clusterCornerAxes(summary_data, cluster_variable = "cluster", features = "avg_log2FC")
# p +
#   theme_minimal() +
#   labs(title = "Cluster Average Expression")

