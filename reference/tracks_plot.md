# Generate a track or heatmap plot based on the provided data

Generate a track or heatmap plot based on the provided data

## Usage

``` r
tracks_plot(
  object = NULL,
  plot_type = c("track", "heatmap"),
  genes = NULL,
  vmin = -2,
  vmax = 2,
  cell_order = NULL,
  gene_order = NULL,
  facet_nested_params = list(),
  theme_params = list(),
  strip_nested_params = list()
)
```

## Arguments

- object:

  An optional object containing the data.

- plot_type:

  The type of plot to generate, either "track" or "heatmap".

- genes:

  A vector or data frame specifying the genes to plot.

- vmin:

  The minimum value for the color scale (only applicable for heatmap).

- vmax:

  The maximum value for the color scale (only applicable for heatmap).

- cell_order:

  An optional vector specifying the order of cells in the plot.

- gene_order:

  An optional vector specifying the order of genes in the plot.

- facet_nested_params:

  A list of additional parameters to customize the facet_nested plot.

- theme_params:

  A list of additional parameters to customize the plot's theme.

- strip_nested_params:

  A list of additional parameters to customize the strip_nested plot.

## Value

A ggplot object representing the track or heatmap plot.

## Author

mixfruit
