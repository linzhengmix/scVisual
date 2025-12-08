# featureCornerAxes: Feature Corner Axes Plot

## Introduction

The `featureCornerAxes` function creates corner axes plots for
visualizing feature-level data. This specialized visualization displays
feature values along the corners of a plot, providing a compact way to
compare features across multiple dimensions.

## Usage

``` r
featureCornerAxes(
  data,
  features,
  group_by = NULL,
  color_by = NULL,
  ...
)
```

## Arguments

- `data`: A data frame containing feature-level data
- `features`: A vector of feature names to plot
- `group_by`: Group features by a variable
- `color_by`: Variable to color the plot by
- `...`: Additional arguments passed to other methods

## Example

### Using Built-in Data

Let’s demonstrate `featureCornerAxes` using the built-in marker data:

``` r
library(scVisual)
data(top3pbmc.markers)

# Create feature corner axes plot
featureCornerAxes(top3pbmc.markers, features = c("avg_log2FC", "pct.1", "pct.2"))
```

### Customizing the Plot

You can customize the feature corner axes plot by adjusting grouping and
colors:

``` r
# Create customized feature corner axes plot
featureCornerAxes(
  top3pbmc.markers,
  features = c("avg_log2FC", "pct.1", "pct.2"),
  group_by = "cluster",
  color_by = "cluster"
)
```

### Focusing on Specific Features

You can create feature corner axes plots for specific sets of features:

``` r
# Plot specific features
featureCornerAxes(
  top3pbmc.markers,
  features = c("avg_log2FC", "p_val_adj"),
  group_by = "cluster",
  title = "Key Feature Comparison"
)
```

## Output

The function returns a ggplot2 object, which can be further customized
using ggplot2 functions:

``` r
# Create feature corner axes plot and customize
p <- featureCornerAxes(
  top3pbmc.markers,
  features = c("avg_log2FC", "pct.1", "pct.2"),
  group_by = "cluster"
)
p + 
  theme_bw() +
  labs(subtitle = "Top 3 Markers per Cluster")
```

## Tips

- Use `featureCornerAxes` to compare features across multiple dimensions
- The function works best with 2-4 features for clear visualization
- Use `group_by` and `color_by` to enhance comparisons across different
  categories
- Adjust the features vector to focus on the most relevant metrics for
  your analysis
