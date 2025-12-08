## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----eval=FALSE---------------------------------------------------------------
# library(scVisual)
# data(top3pbmc.markers)
# 
# # Create feature corner axes plot
# featureCornerAxes(top3pbmc.markers, features = c("avg_log2FC", "pct.1", "pct.2"))


## ----eval=FALSE---------------------------------------------------------------
# # Create customized feature corner axes plot
# featureCornerAxes(
#   top3pbmc.markers,
#   features = c("avg_log2FC", "pct.1", "pct.2"),
#   group_by = "cluster",
#   color_by = "cluster"
# )


## ----eval=FALSE---------------------------------------------------------------
# # Plot specific features
# featureCornerAxes(
#   top3pbmc.markers,
#   features = c("avg_log2FC", "p_val_adj"),
#   group_by = "cluster",
#   title = "Key Feature Comparison"
# )


## ----eval=FALSE---------------------------------------------------------------
# # Create feature corner axes plot and customize
# p <- featureCornerAxes(
#   top3pbmc.markers,
#   features = c("avg_log2FC", "pct.1", "pct.2"),
#   group_by = "cluster"
# )
# p +
#   theme_bw() +
#   labs(subtitle = "Top 3 Markers per Cluster")

