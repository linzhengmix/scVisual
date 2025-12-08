## Installation

``` r
install.packages('devtools')

devtools::install_github('junjunlab/jjAnno')

devtools::install_github('junjunlab/jjPlot')

devtools::install_github("jokergoo/ComplexHeatmap")

devtools::install_github('linzhengmix/scVisual')

# if not install ggunchull
devtools::install_github("sajuukLyu/ggunchull", type = "source")

library(scVisual)
```

## Example

``` r


library(scVisual)

# load test data
test <- system.file("extdata", "seuratTest.RDS", package = "scVisual")
tmp <- readRDS(test)


# umap
clusterCornerAxes(object = tmp,reduction = 'umap',
                  noSplit = T)

# facet by metadata column "orig.ident"
clusterCornerAxes(object = tmp,
                  reduction = 'umap',
                  noSplit = F,
                  groupFacet = 'orig.ident',
                  aspect.ratio = 1,
                  relLength = 0.5)

# retain only one axes
clusterCornerAxes(object = tmp,
                  reduction = 'umap',
                  noSplit = F,
                  groupFacet = 'orig.ident',
                  aspect.ratio = 1,
                  relLength = 0.5,
                  axes = 'one')


# 在调用函数前确保数据干净
prepare_clean_data <- function(seurat_obj, group_column = "orig.ident") {
  # 1. 获取原始数据
  col_data <- seurat_obj@meta.data[[group_column]]

  # 2. 检查并清理问题值
  # 替换NA值
  if (any(is.na(col_data))) {
    cat("发现NA值，将替换为'Unknown'\n")
    col_data[is.na(col_data)] <- "Unknown"
  }

  # 替换空字符串
  if (any(col_data == "")) {
    cat("发现空字符串，将替换为'Unknown'\n")
    col_data[col_data == ""] <- "Unknown"
  }

  # 3. 获取有效的唯一值
  unique_vals <- unique(col_data)
  unique_vals <- unique_vals[unique_vals != "" & !is.na(unique_vals)]

  if (length(unique_vals) == 0) {
    stop("没有有效的分组值")
  }

  # 4. 创建因子
  seurat_obj@meta.data[[group_column]] <- factor(col_data, levels = unique_vals)

  cat("清理完成，因子水平:", paste(levels(seurat_obj@meta.data[[group_column]]), collapse = ", "), "\n")

  return(seurat_obj)
}

# 使用清理后的数据
tmp_clean <- prepare_clean_data(tmp, "orig.ident")

# 现在调用函数
clusterCornerAxes(object = tmp_clean,
                  reduction = 'umap',
                  noSplit = FALSE,
                  groupFacet = 'orig.ident',
                  aspect.ratio = 1,
                  relLength = 0.5,
                  axes = 'one')



# 自定义输出顺序
tmp$orig.ident <- factor(tmp$orig.ident,
                         levels = c("ST2", "ST1", "ST3", "ST4"))

clusterCornerAxes(object = tmp,
                  reduction = 'umap',
                  noSplit = F,
                  groupFacet = 'orig.ident',
                  aspect.ratio = 1,
                  relLength = 0.5,
                  axes = 'one')
```
