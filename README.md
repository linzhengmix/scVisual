<!-- badges: start -->
[![R-CMD-check](https://github.com/linzhengmix/scVisual/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/linzhengmix/scVisual/actions/workflows/R-CMD-check.yaml)
[![zread](https://img.shields.io/badge/Ask_Zread-_.svg?style=flat&color=00b0aa&labelColor=000000&logo=data%3Aimage%2Fsvg%2Bxml%3Bbase64%2CPHN2ZyB3aWR0aD0iMTYiIGhlaWdodD0iMTYiIHZpZXdCb3g9IjAgMCAxNiAxNiIgZmlsbD0ibm9uZSIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj4KPHBhdGggZD0iTTQuOTYxNTYgMS42MDAxSDIuMjQxNTZDMS44ODgxIDEuNjAwMSAxLjYwMTU2IDEuODg2NjQgMS42MDE1NiAyLjI0MDFWNC45NjAxQzEuNjAxNTYgNS4zMTM1NiAxLjg4ODEgNS42MDAxIDIuMjQxNTYgNS42MDAxSDQuOTYxNTZDNS4zMTUwMiA1LjYwMDEgNS42MDE1NiA1LjMxMzU2IDUuNjAxNTYgNC45NjAxVjIuMjQwMUM1LjYwMTU2IDEuODg2NjQgNS4zMTUwMiAxLjYwMDEgNC45NjE1NiAxLjYwMDFaIiBmaWxsPSIjZmZmIi8%2BCjxwYXRoIGQ9Ik00Ljk2MTU2IDEwLjM5OTlIMi4yNDE1NkMxLjg4ODEgMTAuMzk5OSAxLjYwMTU2IDEwLjY4NjQgMS42MDE1NiAxMS4wMzk5VjEzLjc1OTlDMS42MDE1NiAxNC4xMTM0IDEuODg4MSAxNC4zOTk5IDIuMjQxNTYgMTQuMzk5OUg0Ljk2MTU2QzUuMzE1MDIgMTQuMzk5OSA1LjYwMTU2IDE0LjExMzQgNS42MDE1NiAxMy43NTk5VjExLjAzOTlDNS42MDE1NiAxMC42ODY0IDUuMzE1MDIgMTAuMzk5OSA0Ljk2MTU2IDEwLjM5OTlaIiBmaWxsPSIjZmZmIi8%2BCjxwYXRoIGQ9Ik0xMy43NTg0IDEuNjAwMUgxMS4wMzg0QzEwLjY4NSAxLjYwMDEgMTAuMzk4NCAxLjg4NjY0IDEwLjM5ODQgMi4yNDAxVjQuOTYwMUMxMC4zOTg0IDUuMzEzNTYgMTAuNjg1IDUuNjAwMSAxMS4wMzg0IDUuNjAwMUgxMy43NTg0QzE0LjExMTkgNS42MDAxIDE0LjM5ODQgNS4zMTM1NiAxNC4zOTg0IDQuOTYwMVYyLjI0MDFDMTQuMzk4NCAxLjg4NjY0IDE0LjExMTkgMS42MDAxIDEzLjc1ODQgMS42MDAxWiIgZmlsbD0iI2ZmZiIvPgo8cGF0aCBkPSJNNCAxMkwxMiA0TDQgMTJaIiBmaWxsPSIjZmZmIi8%2BCjxwYXRoIGQ9Ik00IDEyTDEyIDQiIHN0cm9rZT0iI2ZmZiIgc3Ryb2tlLXdpZHRoPSIxLjUiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIvPgo8L3N2Zz4K&logoColor=ffffff)](https://zread.ai/linzhengmix/scVisual)
<!-- badges: end -->

## Installation

```R
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

```R
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
