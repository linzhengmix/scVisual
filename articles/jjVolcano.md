# jjVolcano: Enhanced Volcano Plot

## Introduction

The `jjVolcano` function creates enhanced volcano plots for visualizing
differentially expressed genes. It provides additional customization
options compared to standard volcano plots, allowing for more detailed
exploration of gene expression data.

## Usage

``` r
jjVolcano(
  data,
  x = "avg_log2FC",
  y = "p_val_adj",
  group_by = NULL,
  color_by = NULL,
  threshold_x = 0.25,
  threshold_y = 0.05,
  label_top = 10,
  ...
)
```

## Arguments

- `data`: A data frame containing differential expression results
- `x`: Column name for x-axis (log2 fold change, default: “avg_log2FC”)
- `y`: Column name for y-axis (adjusted p-value, default: “p_val_adj”)
- `group_by`: Column name to group data by
- `color_by`: Column name to color data by
- `threshold_x`: Threshold for x-axis values (default: 0.25)
- `threshold_y`: Threshold for y-axis values (default: 0.05)
- `label_top`: Number of top genes to label (default: 10)
- `...`: Additional arguments passed to other methods

## Example

### Using Built-in Data

Let’s demonstrate `jjVolcano` using the built-in PBMC marker data:

``` r
library(scVisual)
data(pbmc.markers)

# Create enhanced volcano plot
jjVolcano(pbmc.markers, group_by = "cluster")
```

### Customizing the Plot

You can customize the volcano plot by adjusting thresholds, color
schemes, and labeling:

``` r
# Create customized volcano plot
jjVolcano(
  pbmc.markers,
  group_by = "cluster",
  threshold_x = 0.5,
  threshold_y = 0.01,
  label_top = 5,
  color_by = "cluster"
)
```

### Focusing on Specific Clusters

You can filter the data to focus on specific clusters:

``` r
# Focus on specific clusters
naive_cd4_markers <- pbmc.markers[pbmc.markers$cluster %in% c("Naive CD4 T", "Memory CD4 T"), ]
jjVolcano(naive_cd4_markers, group_by = "cluster")
```

## Output

The function returns a ggplot2 object, which you can further customize
using ggplot2 functions:

``` r
# Create volcano plot and customize
p <- jjVolcano(pbmc.markers, group_by = "cluster")
p + 
  theme_minimal() +
  labs(title = "Enhanced Volcano Plot")
```

## Tips

- Use `group_by` to compare multiple groups in the same plot
- Use `color_by` to highlight different categories
- Adjust `threshold_x` and `threshold_y` to focus on genes of interest
- Use `label_top` to label the most significant genes
