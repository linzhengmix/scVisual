# Create a scatter cell plot using the grid package

Create a scatter cell plot using the grid package

## Usage

``` r
scatter_cell_plot(
  object = NULL,
  color = NULL,
  dim = "umap",
  rm_axis = FALSE,
  cell_id = NULL,
  bar_width = 0.2,
  point_size = 1,
  rm_barplot = FALSE,
  legend_psize = 1.5,
  arrow_len = 0.2
)
```

## Arguments

- object:

  A Seurat object containing the data.

- color:

  A vector of colors for each cell type. If NULL, random colors will be
  assigned.

- dim:

  The dimension used for plotting (default is "umap").

- rm_axis:

  Logical value indicating whether to remove the x and y-axis (default
  is FALSE).

- cell_id:

  Name of the column in the metadata that represents cell identity
  (default is NULL).

- bar_width:

  Width of the barplot (default is 0.2).

- point_size:

  Size of the points in the scatter plot (default is 1).

- rm_barplot:

  Whether to remove barplot, default FALSE.

- legend_psize:

  Legend point size, default 1.5.

- arrow_len:

  Arrow length, default 0.2.

## Value

None

## Author

mixfruit

## Examples

``` r
if (FALSE) { # \dontrun{
scatter_cell_plot(object = seurat_object)
} # }
```
