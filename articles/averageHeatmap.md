# averageHeatmap: Average Expression Heatmap

## Introduction

The `averageHeatmap` function creates heatmaps displaying average gene
expression levels across clusters. This visualization is useful for
identifying patterns of gene expression across different cell
populations.

## Usage

``` r
averageHeatmap(
  object,
  features,
  group_by = NULL,
  scale = TRUE,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  ...
)
```

## Arguments

- `object`: A Seurat object containing gene expression data
- `features`: A vector of feature names to include in the heatmap
- `group_by`: Group cells by a metadata field
- `scale`: Whether to scale the data (default: TRUE)
- `cluster_rows`: Whether to cluster rows (default: TRUE)
- `cluster_cols`: Whether to cluster columns (default: TRUE)
- `...`: Additional arguments passed to ComplexHeatmap::Heatmap

## Example

### Using Built-in Data

Let’s demonstrate `averageHeatmap` using the built-in marker data:

``` r
library(scVisual)
data(top3pbmc.markers)

# Load Seurat object (example)
# seurat_obj <- readRDS(system.file("extdata", "seuratTest.RDS", package = "scVisual"))

# Create average expression heatmap
# averageHeatmap(seurat_obj, features = unique(top3pbmc.markers$gene))
```

### Customizing the Plot

You can customize the heatmap by adjusting clustering, scaling, and
grouping:

``` r
# Create customized heatmap
# averageHeatmap(
#   seurat_obj,
#   features = unique(top3pbmc.markers$gene),
#   group_by = "orig.ident",
#   scale = FALSE,
#   cluster_rows = FALSE
# )
```

### Plotting Selected Features

You can create heatmaps for specific sets of features:

``` r
# Plot specific genes
# averageHeatmap(
#   seurat_obj,
#   features = c("CD3E", "CD14", "MS4A1", "CD8A", "FCGR3A"),
#   title = "Key Immune Cell Markers"
# )
```

## Output

The function returns a ComplexHeatmap object, which can be further
customized using ComplexHeatmap functions:

``` r
# Create heatmap and customize
# p <- averageHeatmap(seurat_obj, features = unique(top3pbmc.markers$gene))
# p + ComplexHeatmap::rowAnnotation(gene_type = "marker")
```

## Tips

- Use `scale = TRUE` to normalize gene expression values across clusters
- Adjust `cluster_rows` and `cluster_cols` to control clustering
  behavior
- Use `group_by` to compare expression patterns across different
  metadata groups
- Consider reducing the number of features for better visualization
