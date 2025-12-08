# scVisual: Single-cell Visualization Tools

## Introduction

scVisual is an R package providing various visualization tools for
single-cell RNA-seq data analysis. It includes functions for creating
volcano plots, dot plots, heatmaps, and other specialized visualizations
for single-cell data.

## Installation

You can install scVisual from GitHub using:

``` r
# install.packages("devtools")
devtools::install_github("sajuukLyu/scVisual")
```

## Data

scVisual comes with two built-in datasets:

1.  `pbmc.markers`: Marker genes identified from PBMC dataset
2.  `top3pbmc.markers`: Top 3 marker genes per cluster from PBMC dataset

Let’s take a quick look at these datasets:

``` r
library(scVisual)
data(pbmc.markers)
data(top3pbmc.markers)

# Structure of pbmc.markers
str(pbmc.markers, max.level = 1)
#> 'data.frame':    5921 obs. of  7 variables:
#>  $ p_val     : num  2.01e-140 2.62e-140 1.28e-138 4.36e-135 3.62e-128 ...
#>  $ avg_log2FC: num  0.726 0.724 0.674 0.612 0.618 ...
#>  $ pct.1     : num  1 0.999 1 0.999 1 0.661 0.997 0.994 0.677 1 ...
#>  $ pct.2     : num  0.991 0.992 0.995 0.995 0.994 0.914 0.975 0.972 0.904 0.996 ...
#>  $ p_val_adj : num  2.75e-136 3.60e-136 1.76e-134 5.98e-131 4.96e-124 ...
#>  $ cluster   : Factor w/ 9 levels "Naive CD4 T",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ gene      : chr  "RPS12" "RPS27" "RPS6" "RPL32" ...

# Structure of top3pbmc.markers
str(top3pbmc.markers, max.level = 1)
#> gropd_df [27 × 7] (S3: grouped_df/tbl_df/tbl/data.frame)
#>  - attr(*, "groups")= tibble [9 × 2] (S3: tbl_df/tbl/data.frame)
#>   ..- attr(*, ".drop")= logi TRUE
```

## Available Functions

scVisual provides the following visualization functions:

1.  `averageHeatmap`: Create average expression heatmap
2.  `cellRatioPlot`: Plot cell ratios across clusters
3.  `clusterCornerAxes`: Plot cluster data with corner axes
4.  `drawLegend`: Draw custom legends
5.  `featureCornerAxes`: Plot feature data with corner axes
6.  `featurePlot`: Plot feature expression
7.  `jjDotPlot`: Create dot plots
8.  `jjVolcano`: Create volcano plots
9.  `markerVolcano`: Create volcano plots for markers
10. `scatterCellPlot`: Create scatter plots for cells
11. `tracksPlot`: Create tracks plots

## Getting Started

To get started with scVisual, load the package and the built-in data:

``` r
library(scVisual)
data(pbmc.markers)
data(top3pbmc.markers)
```

You can then use any of the visualization functions with your data. For
example, to create a volcano plot:

``` r
markerVolcano(pbmc.markers, cluster = "Naive CD4 T")
```

## Function Vignettes

For detailed usage of each function, please refer to their respective
vignettes:

- [averageHeatmap](averageHeatmap.md)
- [cellRatioPlot](cellRatioPlot.md)
- [clusterCornerAxes](clusterCornerAxes.md)
- [drawLegend](drawLegend.md)
- [featureCornerAxes](featureCornerAxes.md)
- [featurePlot](featurePlot.md)
- [jjDotPlot](jjDotPlot.md)
- [jjVolcano](jjVolcano.md)
- [markerVolcano](markerVolcano.md)
- [scatterCellPlot](scatterCellPlot.md)
- [tracksPlot](tracksPlot.md)
