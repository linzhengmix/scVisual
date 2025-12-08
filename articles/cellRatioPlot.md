# cellRatioPlot: Cell Ratio Plot

## Introduction

The `cellRatioPlot` function creates bar plots showing the ratio of
cells across different clusters or groups. This visualization is useful
for comparing the distribution of cell types across different samples or
conditions.

## Usage

``` r
cellRatioPlot(
  object,
  group_by = NULL,
  split_by = NULL,
  normalize = TRUE,
  ...
)
```

## Arguments

- `object`: A Seurat object containing cell metadata
- `group_by`: Group cells by a metadata field
- `split_by`: Split the plot by a metadata field
- `normalize`: Whether to normalize the data (default: TRUE)
- `...`: Additional arguments passed to ggplot2::geom_bar

## Example

### Using Built-in Data

Let’s demonstrate `cellRatioPlot` using example data:

``` r
library(scVisual)

# Load Seurat object (example)
# seurat_obj <- readRDS(system.file("extdata", "seuratTest.RDS", package = "scVisual"))

# Create cell ratio plot
# cellRatioPlot(seurat_obj, group_by = "orig.ident")
```

### Customizing the Plot

You can customize the cell ratio plot by adjusting normalization,
grouping, and splitting:

``` r
# Create customized cell ratio plot
# cellRatioPlot(
#   seurat_obj,
#   group_by = "orig.ident",
#   split_by = "seurat_clusters",
#   normalize = FALSE
# )
```

### Comparing Conditions

Cell ratio plots are particularly useful for comparing cell
distributions across different conditions:

``` r
# Compare cell ratios across conditions
# cellRatioPlot(
#   seurat_obj,
#   group_by = "condition",
#   split_by = "seurat_clusters",
#   title = "Cell Type Distribution Across Conditions"
# )
```

## Output

The function returns a ggplot2 object, which can be further customized
using ggplot2 functions:

``` r
# Create cell ratio plot and customize
# p <- cellRatioPlot(seurat_obj, group_by = "orig.ident")
# p + 
#   theme_bw() +
#   labs(subtitle = "Cell Distribution") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

## Tips

- Use `normalize = TRUE` to compare proportions across groups of
  different sizes
- Use `split_by` to show the composition of each group
- Adjust colors and themes to improve readability
