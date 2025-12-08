# This function creates a scatter plot for multiple genes or features from a Seurat object.

This function creates a scatter plot for multiple genes or features from
a Seurat object.

## Arguments

- object:

  A Seurat object containing the data.

- reduction:

  The dimension to use for plotting, default is "umap".

- genes:

  A character vector of gene names or feature names to be plotted.

- nrow:

  Number of rows in the plot grid.

- ncol:

  Number of columns in the plot grid.

- quantile.val:

  The quantile value to determine the color cutoff for each gene.

- color:

  A vector of colors to be used for plotting, defaults to a predefined
  set of colors.

- rm_axis:

  Logical value indicating whether to remove axis labels and ticks,
  defaults to FALSE.

- rm_legend:

  Logical value indicating whether to remove the color legend, defaults
  to FALSE.

- add_rect:

  Logical value indicating whether to add a rectangle around each plot
  panel, defaults to FALSE.

- add_cor_arrow:

  Logical value indicating whether to add arrows indicating the
  correlation direction, defaults to FALSE.

- add_strip:

  Logical value indicating whether to add a strip at the top of each
  plot panel, defaults to FALSE.

- cor_label_dist:

  Distance between the corner arrows and the axis labels.

- arrow_len:

  Length of the corner arrows.

- arrow_label_size:

  Font size of the corner arrow labels.

- plot_size:

  Size of each individual scatter plot.

- keep_one_cor:

  Logical value indicating whether to keep only one set of corner arrows
  across the entire plot grid, defaults to FALSE.

- xlab:

  Label for the x-axis.

- ylab:

  Label for the y-axis.

- respect:

  Logical value indicating whether to respect the specified number of
  rows and columns in the plot grid, defaults to TRUE.

- point_size:

  the point size, default 1.

## Author

mixfruit

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming "seurat_obj" is a Seurat object
featurePlot(object = seurat_obj, genes = c("gene1", "gene2", "gene3"), nrow = 2, ncol = 2)
} # }
```
