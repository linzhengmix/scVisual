# drawLegend: Custom Legend Drawing

## Introduction

The `drawLegend` function creates custom legends for visualizations.
This utility function allows you to create highly customized legends
that can be added to other plots, providing more flexibility than
default ggplot2 legends.

## Usage

``` r
drawLegend(
  colors,
  labels,
  title = NULL,
  orientation = "vertical",
  ...
)
```

## Arguments

- `colors`: A vector of colors for the legend
- `labels`: A vector of labels corresponding to the colors
- `title`: Legend title (default: NULL)
- `orientation`: Legend orientation (“vertical” or “horizontal”,
  default: “vertical”)
- `...`: Additional arguments passed to other methods

## Example

### Basic Usage

Let’s demonstrate `drawLegend` with a simple example:

``` r
library(scVisual)

# Create custom legend
legend <- drawLegend(
  colors = c("#FF0000", "#00FF00", "#0000FF"),
  labels = c("Cluster 1", "Cluster 2", "Cluster 3"),
  title = "Clusters",
  orientation = "horizontal"
)

# Display the legend
legend
```

### Combining with Other Plots

You can use `drawLegend` to create custom legends for other
visualizations:

``` r
library(scVisual)
library(ggplot2)

data(pbmc.markers)

# Create a basic plot
p <- ggplot(pbmc.markers[pbmc.markers$cluster == "Naive CD4 T", ], 
            aes(x = avg_log2FC, y = -log10(p_val_adj)))
  geom_point()
  labs(title = "Volcano Plot")

# Create custom legend
legend <- drawLegend(
  colors = c("#374E55", "#DF8F44", "#00A1D5"),
  labels = c("Not Significant", "LogFC > 0.25", "p-adj < 0.05"),
  title = "Significance"
)

# Combine plot and legend (example)
# gridExtra::grid.arrange(p, legend, ncol = 2, widths = c(4, 1))
```

### Customizing the Legend

You can customize the legend appearance by adjusting various parameters:

``` r
# Create highly customized legend
drawLegend(
  colors = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF"),
  labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"),
  title = "Cell Clusters",
  orientation = "vertical",
  size = 10,
  font_size = 12
)
```

## Output

The function returns a ggplot2 object representing the custom legend,
which can be further customized using ggplot2 functions:

``` r
# Create and customize legend
legend <- drawLegend(
  colors = c("#FF0000", "#00FF00", "#0000FF"),
  labels = c("A", "B", "C")
)

# Further customize with ggplot2
legend + 
  theme(legend.title = element_text(face = "bold", size = 14))
```

## Tips

- Use `drawLegend` when you need more flexibility than default ggplot2
  legends
- Adjust `orientation` to place the legend horizontally or vertically
- Combine with `gridExtra` or similar packages to arrange legends with
  other plots
- Use consistent color schemes between your main plot and custom legend
