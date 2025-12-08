# Add corner axes on seurat UMAP/tSNE cluster figures

Add corner axes on seurat UMAP/tSNE cluster figures

## Arguments

- object:

  seurat object.

- reduction:

  "string", reduction type (umap/tsne).

- group_facet:

  "string", give the column name in seurat metadata to facet plot.

- cluster_col:

  "string", the point color to group by,cluster name, default
  "seurat_clusters".

- p_size:

  "num", point size.

- aspect_ratio:

  "num", plot width and height ratio, default NULL.

- no_split:

  "logic", whether to split/facet the plot, default "TRUE".

- nrow:

  "num", rows to plot when no_split = FALSE.

- rel_length:

  "num", the corner axis line relative length to plot axis(0-1).

- rel_dist:

  "num" ,the relative distance of corner axis label to axis.

- axes:

  "string", show multiple corner axis or only one (mul/one), default
  "mul".

- legend_pos:

  "string", legend position same as ggplot theme function, default
  "right".

- key_size:

  The legned point size, default is 5.

- line_text_col:

  "string", corner line and label color, default "black".

- strip_col:

  "string", facet background color, default "white".

- arrow_type:

  "string", arrow type (open/closed), default "closed".

- corner_text_size:

  "num", the corner label text size, default is 3.

- base_size:

  "num", theme base size, default is 14.

- theme_bg:

  Another theme style, default is "default", or "bwCorner".

- add_circle:

  "logic", whether add circle on clusters, default is "FALSE".

- add_circle_legacy:

  "logic", using the legacy version to add a circle, the parameters
  \`nbin\`, \`nsm\`, \`addsm\`, \`sfac\` and \`qval\` are only
  applicable to legacy, default is "FALSE".

- cic_alpha:

  "num", circle fill color alpha, default is 0.1.

- cic_delta:

  "num", the distance to extend the curve (circle), this parameter only
  takes effect when \`add_circle_legacy = FALSE\`.

- cic_line_size:

  "num", circle line size, default is 1.

- cic_line_color:

  "num", circle line color, default is "grey50".

- cic_line_lty:

  "num", circle line type, default is "dashed".

- nbin:

  "num", number of points used to shape the hull, default 100.

- nsm:

  "num", number of points used to perform convolution, should less than
  nbin, default 10.

- addsm:

  "num", number of additional times of convolution performed, default 1.

- qval:

  "num", expansion size factor, larger value means bigger hull, default
  1.5.

- sfac:

  "num", quantile of each sector, used to determine the edge of the
  hull, should less than 1, default 1.5.

- cell_label:

  Whether to label cell type on plot, default is FALSE.

- cell_label_size:

  Cell type label size, default is 6.

- cell_label_color:

  Cell type label color, default is "black".

- show_legend:

  Whether show legend, default is TRUE.

- corner_variable:

  Which group corner axis to be added when "axes" set to "one", default
  is the first level.

## Value

Return a ggplot object.

## Author

mixfruit

## Examples

``` r
test <- system.file("extdata", "seuratTest.RDS", package = "scVisual")

tmp <- readRDS(test)

# umap
clusterCornerAxes(
  object = tmp, reduction = "umap",
  no_split = TRUE
)
#> Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
#> ℹ Please use tidy evaluation idioms with `aes()`.
#> ℹ See also `vignette("ggplot2-in-packages")` for more information.
#> ℹ The deprecated feature was likely used in the scVisual package.
#>   Please report the issue to the authors.


# arrowType
clusterCornerAxes(
  object = tmp, reduction = "umap",
  no_split = TRUE, arrow_type = "open"
)


# facet by metadata column "orig.ident"
clusterCornerAxes(
  object = tmp, reduction = "umap",
  no_split = FALSE, group_facet = "orig.ident",
  rel_length = 0.5
)


# retain only one axes with factor ordering
tmp$orig.ident <- factor(tmp$orig.ident, levels = c("ST2", "ST3", "ST1", "ST4"))
clusterCornerAxes(
  object = tmp, reduction = "umap",
  no_split = FALSE, group_facet = "orig.ident",
  rel_length = 0.5,
  axes = "one"
)


# line color
clusterCornerAxes(
  object = tmp, reduction = "umap",
  no_split = FALSE, group_facet = "orig.ident",
  rel_length = 0.5,
  line_text_col = "grey50"
)


# tsne
clusterCornerAxes(
  object = tmp, reduction = "tsne",
  no_split = FALSE, group_facet = "orig.ident",
  rel_length = 0.5
)
```
