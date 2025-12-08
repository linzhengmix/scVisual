# Marker genes volcano plot

Marker genes volcano plot

## Arguments

- markers:

  Dataframe marker genes from findAllmarkers function from seurat.

- own_gene:

  Your own gene names to be labeled on plot, defaults is null.

- top_n:

  Numbers top genes to label, defaults is 5.

- log2fc:

  The threshold of log2FC, defaults is 0.25.

- hline_size:

  Hline size, defaults is 1.

- hline_color:

  Hline color, defaults is "grey50".

- p_force:

  Positive gene force parameters to avoid overlap gene labels, defaults
  is 5.

- n_force:

  Negative gene force parameters to avoid overlap gene labels, defaults
  is 2.5.

- nudge_x:

  Adjustments on the horizontal of the gene label, defaults is 0.8.

- p_nudge_y:

  Adjustments on the horizontal of the positive gene label, defaults is
  0.25.

- n_nudge_y:

  Adjustments on the horizontal of the negative gene label, defaults is
  0.

- base_size:

  Theme base size, defaults is 14.

- facet_color:

  Facet border color, defaults is NA.

- facet_fill:

  Facet fill color, defaults is "white".

- ylab:

  Plot y label, defaults is "Log2-Fold Change".

- nrow:

  Numbers rows to plot, defaults is 1.

## Value

Return a ggplot.

## Author

mixfruit

## Examples

``` r
test <- system.file("extdata", "pbmc.markers.csv", package = "scVisual")
markers <- read.csv(test)

marker_volcano(
  markers = markers,
  top_n = 5,
  label_col = ggsci::pal_npg()(9)
)
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the scVisual package.
#>   Please report the issue to the authors.
```
