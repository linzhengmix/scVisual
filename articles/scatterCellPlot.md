# scatterCellPlot: Scatter Plot for Cells

## Introduction

The `scatterCellPlot` function creates scatter plots for visualizing
cell-level data. This visualization is useful for examining
relationships between variables across cells, such as gene expression
levels or metadata attributes.

## Usage

``` r
scatterCellPlot(
  object,
  x,
  y,
  color_by = NULL,
  group_by = NULL,
  ...
)
```

## Arguments

- `object`: A Seurat object containing cell-level data
- `x`: Variable for x-axis
- `y`: Variable for y-axis
- `color_by`: Variable to color points by
- `group_by`: Group cells by a variable
- `...`: Additional arguments passed to ggplot2::geom_point

## Example

### Using Built-in Data

Let’s demonstrate `scatterCellPlot` using example data:

``` r
library(scVisual)

# Load Seurat object (example)
# seurat_obj <- readRDS(system.file("extdata", "seuratTest.RDS", package = "scVisual"))

# Create scatter plot of gene expression
# scatterCellPlot(seurat_obj, x = "CD3E", y = "CD4", color_by = "seurat_clusters")
```

### Customizing the Plot

You can customize the scatter plot by adjusting point size,
transparency, and color scheme:

``` r
# Create customized scatter plot
# scatterCellPlot(
#   seurat_obj,
#   x = "CD3E",
#   y = "CD4",
#   color_by = "seurat_clusters",
#   group_by = "orig.ident",
#   alpha = 0.7,
#   size = 2
# )
```

### Plotting Metadata

You can also use `scatterCellPlot` to visualize relationships between
metadata variables:

``` r
# Plot metadata relationships
# scatterCellPlot(
#   seurat_obj,
#   x = "nCount_RNA",
#   y = "nFeature_RNA",
#   color_by = "seurat_clusters",
#   title = "UMI Count vs. Feature Count"
# )
```

## Output

The function returns a ggplot2 object, which can be further customized
using ggplot2 functions:

``` r
# Create scatter plot and customize
# p <- scatterCellPlot(seurat_obj, x = "CD3E", y = "CD4", color_by = "seurat_clusters")
# p + 
#   theme_minimal() +
#   labs(subtitle = "T Cell Markers")
```

## Tips

- Use `color_by` to highlight different cell groups or clusters
- Adjust `alpha` to improve visibility when plotting many cells
- Use `size` to control point size for better readability
- Combine with ggplot2 layers like `geom_smooth` to add trend lines
