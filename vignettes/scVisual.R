## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----eval=FALSE---------------------------------------------------------------
# # install.packages("devtools")
# devtools::install_github("sajuukLyu/scVisual")


## -----------------------------------------------------------------------------
library(scVisual)
data(pbmc.markers)
data(top3pbmc.markers)

# Structure of pbmc.markers
str(pbmc.markers, max.level = 1)

# Structure of top3pbmc.markers
str(top3pbmc.markers, max.level = 1)


## -----------------------------------------------------------------------------
library(scVisual)
data(pbmc.markers)
data(top3pbmc.markers)


## ----eval=FALSE---------------------------------------------------------------
# markerVolcano(pbmc.markers, cluster = "Naive CD4 T")

