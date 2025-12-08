# jjDotPlot: Dot Plot for Gene Expression

## Introduction

The `jjDotPlot` function creates dot plots for visualizing gene
expression across clusters. Dot plots are effective for displaying both
the average expression level and the percentage of cells expressing a
gene within each cluster.

## Usage

``` r
jjDotPlot(
  object,
  features,
  cols = c("lightgrey", "blue"),
  dot.min = 0.05,
  dot.scale = 6,
  idents = NULL,
  group.by = NULL,
  split.by = NULL,
  ...
)
```

## Arguments

- `object`: A Seurat object containing gene expression data
- `features`: A vector of feature names to plot
- `cols`: A vector of colors for low and high expression values
- `dot.min`: Minimum value for dot plotting (default: 0.05)
- `dot.scale`: Scale factor for dot size (default: 6)
- `idents`: Identities to include in the plot
- `group.by`: Group cells by a metadata field
- `split.by`: Split the plot by a metadata field
- `...`: Additional arguments passed to other methods

## Example

### Using Built-in Data

While `jjDotPlot` typically works with Seurat objects, we can
demonstrate its usage with the built-in marker data:

``` r
library(scVisual)
data(top3pbmc.markers)

# Load Seurat object (example)
# seurat_obj <- readRDS(system.file("extdata", "seuratTest.RDS", package = "scVisual"))

# Create dot plot for top marker genes
# jjDotPlot(seurat_obj, features = unique(top3pbmc.markers$gene))
```

### Customizing the Plot

You can customize the dot plot by adjusting color scheme, dot size, and
grouping:

``` r
# Create customized dot plot
# jjDotPlot(
#   seurat_obj,
#   features = unique(top3pbmc.markers$gene),
#   cols = c("white", "red"),
#   dot.scale = 8,
#   group.by = "orig.ident"
# )
```

### Plotting Selected Features

You can plot specific features of interest:

``` r
# Plot specific genes
# jjDotPlot(
#   seurat_obj,
#   features = c("CD3E", "CD14", "MS4A1"),
#   title = "Key Immune Cell Markers"
# )
```

## Output

The function returns a ggplot2 object, which you can further customize
using ggplot2 functions:

``` r
# Create dot plot and customize
# p <- jjDotPlot(seurat_obj, features = unique(top3pbmc.markers$gene))
# p + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(title = "Top Marker Genes")
```

## Tips

- Use `cols` to adjust the color scheme for expression levels
- Use `dot.scale` to make dots larger or smaller
- Use `group.by` to group cells by different metadata fields
- Use `split.by` to create separate plots for different groups
- Consider using `features` to select a manageable number of genes for
  clarity
