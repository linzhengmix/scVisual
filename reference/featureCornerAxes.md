# Add corner axes on seurat UMAP/tSNE gene FeaturePlot function figures

Add corner axes on seurat UMAP/tSNE gene FeaturePlot function figures

## Arguments

- object:

  object seurat object.

- reduction:

  "string", reduction type (umap/tsne).

- features:

  "string", the gene you want to plot.

- group_facet:

  "string", give the column name in seurat metadata to facet plot, if it
  is "NULL", facet plot only by gene.

- rel_length:

  "num", the corner axis line relative length to plot axis(0-1).

- rel_dist:

  "num", the relative distance of corner axis label to axis.

- aspect_ratio:

  "num", plot width and height ratio, default NULL.

- low:

  "string", point color with low expression.

- high:

  "string", point color with high expression.

- axes:

  "string", show multiple corner axis or only one (mul/one), default
  "mul".

- legend_pos:

  "string", legend position same as ggplot theme function, default
  "right".

- strip_col:

  "string", facet background color, defaults "white".

- p_size:

  "num", point size.

- arrow_type:

  "string", arrow type (open/closed), default "closed".

- line_text_col:

  "string", facet background color, default "white".

- corner_text_size:

  "num", the corner label text size, default is 5.

- base_size:

  "num", theme base size, default is 14.

- theme_bg:

  Another theme style, default is "default", or "bwCorner".

- show.legend:

  Whether show legend, default is "TRUE".

- cornerVariable:

  Which group corner axis to be added when "axes" set to "one", default
  is the first group.

- n_layout:

  = NULL Similar to the ncol/nrow for the layout, default is the gene
  numbers.

- min_exp:

  Minimum expression value defined, default is NULL.

- max_exp:

  Maxmum expression value defined, default is NULL.

## Value

Return a ggplot.

## Author

mixfruit

## Examples

``` r
test <- system.file("extdata", "seuratTest.RDS", package = "scVisual")

tmp <- readRDS(test)

# umap
featureCornerAxes(
  object = tmp, reduction = "umap",
  group_facet = "orig.ident",
  rel_length = 0.5, rel_dist = 0.2,
  features = c("Actb", "Ythdc1", "Ythdf2")
)
#> Error in featureCornerAxes(object = tmp, reduction = "umap", group_facet = "orig.ident",     rel_length = 0.5, rel_dist = 0.2, features = c("Actb", "Ythdc1",         "Ythdf2")): unused arguments (group_facet = "orig.ident", rel_length = 0.5, rel_dist = 0.2)

# one axes with factor ordering
tmp$orig.ident <- factor(tmp$orig.ident, levels = c("ST2", "ST3", "ST1", "ST4"))
featureCornerAxes(
  object = tmp, reduction = "umap",
  group_facet = "orig.ident",
  features = c("Actb", "Ythdc1", "Ythdf2"),
  rel_length = 0.5, rel_dist = 0.2,
  axes = "one",
  line_text_col = "grey50"
)
#> Error in featureCornerAxes(object = tmp, reduction = "umap", group_facet = "orig.ident",     features = c("Actb", "Ythdc1", "Ythdf2"), rel_length = 0.5,     rel_dist = 0.2, axes = "one", line_text_col = "grey50"): unused arguments (group_facet = "orig.ident", rel_length = 0.5, rel_dist = 0.2, line_text_col = "grey50")

# tsne
featureCornerAxes(
  object = tmp, reduction = "tsne",
  group_facet = "orig.ident",
  rel_length = 0.5, rel_dist = 0.2,
  features = c("Actb", "Ythdc1", "Ythdf2")
)
#> Error in featureCornerAxes(object = tmp, reduction = "tsne", group_facet = "orig.ident",     rel_length = 0.5, rel_dist = 0.2, features = c("Actb", "Ythdc1",         "Ythdf2")): unused arguments (group_facet = "orig.ident", rel_length = 0.5, rel_dist = 0.2)
```
