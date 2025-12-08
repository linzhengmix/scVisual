## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----eval=FALSE---------------------------------------------------------------
# library(scVisual)
# 
# # Load example data (hypothetical)
# # tracks_data <- readRDS(system.file("extdata", "htdata.RDS", package = "scVisual"))
# 
# # Create tracks plot
# # tracksPlot(tracks_data, tracks = c("track1", "track2", "track3"))


## ----eval=FALSE---------------------------------------------------------------
# # Create customized tracks plot
# # tracksPlot(
# #   tracks_data,
# #   tracks = c("track1", "track2", "track3"),
# #   group_by = "condition",
# #   color_by = "sample",
# #   title = "Genomic Tracks Comparison"
# # )


## ----eval=FALSE---------------------------------------------------------------
# # Create tracks plot and customize
# # p <- tracksPlot(tracks_data, tracks = c("track1", "track2", "track3"))
# # p +
# #   theme_minimal() +
# #   labs(subtitle = "Multi-omic Tracks")

