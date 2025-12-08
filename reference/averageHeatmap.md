# Plot averaged gene expression cross cluster cells

Plot averaged gene expression cross cluster cells

## Usage

``` r
averageHeatmap(
  object = NULL,
  markerGene = NULL,
  group.by = "ident",
  assays = "RNA",
  slot = "data",
  htCol = c("#0099CC", "white", "#CC0033"),
  colseed = 666,
  htRange = c(-2, 0, 2),
  annoCol = FALSE,
  myanCol = NULL,
  annoColType = "light",
  annoColTypeAlpha = 0,
  row_title = "Cluster top Marker genes",
  row_names_side = "left",
  border = FALSE,
  fontsize = 10,
  column_names_rot = 45,
  showRowNames = TRUE,
  markGenes = NULL,
  clusterAnnoName = TRUE,
  width = NULL,
  height = NULL,
  cluster.order = NULL,
  cluster_columns = FALSE,
  cluster_rows = FALSE,
  gene.order = NULL,
  ...
)
```

## Arguments

- object:

  object seurat object.

- markerGene:

  Your marker genes.

- group.by:

  Categories for grouping (e.g, ident, replicate, celltype). "ident" by
  default.

- assays:

  Which assays to use. Default is "RNA" assays.

- slot:

  Slot(s) to use. Default is "data".

- htCol:

  Heatmap colors. Default is c("#0099CC", "white", "#CC0033").

- colseed:

  Cluster annotation colors seed, these colors are produced randomly, so
  you can give a seed to assure produce same colors. Default is 666.

- htRange:

  Heatmap values range. Default is c(-2, 0, 2).

- annoCol:

  Whether use your own annotation clusters colors. Default is "FALSE".

- myanCol:

  You can specify your own annotation clusters colors vectors. Default
  is "null".

- annoColType:

  Cluster annotation colors type (bright, light, dark and random).
  Default is light.

- annoColTypeAlpha:

  Cluster annotation colors transparency. Default is 0.

- row_title:

  Heatmap row title. Default is "Cluster top Marker genes".

- row_names_side:

  Heatmap gene name side. Default is "left".

- border:

  Whether to shOw heatmap border. Default is "FALSE".

- fontsize:

  Heatmap gene name fontsize. Default is 10.

- column_names_rot:

  Cluster name rotation. Default is 45.

- showRowNames:

  whether to show rownames. Default is "TRUE".

- markGenes:

  Provide your target genes to mark on the plot. Default is "NULL".

- clusterAnnoName:

  Whether to add clsuetr column annotation name. Default is "TRUE".

- width:

  The heatmap body width. Default is "NULL".

- height:

  The heatmap body height. Default is "NULL".

- cluster.order:

  The cell clusters order. Default is "NULL".

- cluster_columns:

  Whether cluster columns. Default is "FALSE".

- cluster_rows:

  Whether cluster rows. Default is "FALSE".

- gene.order:

  the gene orders for heatmap. Default is "NULL".

- ...:

  Other arguments passed with ComplexHeatmap::rowAnnotation and
  ComplexHeatmap::Heatmap.

## Value

Return a plot.

## Author

mixfruit

## Examples

``` r
if (FALSE) { # \dontrun{
httest <- system.file("extdata", "htdata.RDS", package = "scVisual")
pbmc <- readRDS(httest)

# load markergene
markergene <- system.file("extdata", "top5pbmc.markers.csv", package = "scVisual")
markers <- read.table(markergene, sep = ",", header = TRUE)

# plot
averageHeatmap(
  object = pbmc,
  markerGene = markers$gene
)

# change color
averageHeatmap(
  object = pbmc,
  markerGene = markers$gene,
  htCol = c("#339933", "#FFCC00", "#FF0033")
)
} # }
```
