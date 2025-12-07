#' @name jjDotPlot
#' @author mixfruit

#' @title Dot Plot for Gene Expression Visualization
#' @param object Seurat object containing the data.
#' @param gene Genes to plot.
#' @param markerGene Marker genes with cell type info.
#' @param ... Additional parameters.
#'
#' Create a dot plot of average expression and percent expressing
#' across clusters or groups.
#' @return A ggplot object.
#' @export
#'


# Define global variables
globalVariables(c("%||%", ".", "avg.exp", "avg.exp.scaled", "celltype", "group", "pct.exp", "unit"))

# Get PercentAbove function from Seurat
PercentAbove <- utils::getFromNamespace("PercentAbove", "Seurat")

# Define the function
jjDotPlot <- function(
    object = NULL,
    assay = NULL,
    slot = "data",
    id = "seurat_clusters",
    split.by = NULL,
    split.by.aesGroup = FALSE,
    gene = NULL,
    markerGene = NULL,
    gene.order = NULL,
    cluster.order = NULL,
    point.geom = TRUE,
    point.shape = 21,
    tile.geom = FALSE,
    dot.col = c("white", "#990000"),
    midpoint = 0.5,
    scale = TRUE,
    col.min = -2.5,
    col.max = 2.5,
    rescale = FALSE,
    rescale.min = 0,
    rescale.max = 1,
    dot.min = 1,
    dot.max = 6,
    base_size = 14,
    x.text.angle = 90,
    x.text.hjust = 1,
    x.text.vjust = 0,
    ytree = TRUE,
    xtree = FALSE,
    tree.pos = NULL,
    same.pos.label = FALSE,
    size.width = 0.3,
    bar.width = 4.5,
    plot.margin = c(1, 1, 1, 1),
    anno = FALSE,
    aesGroName = "cluster",
    segWidth = 0.8,
    lwd = 3,
    textRot = 90,
    textSize = 14,
    hjust = 0,
    legend.position = "right",
    bar.legendTitle = "Mean expression \n in group",
    point.legendTitle = "Fraction of cells \n in group (%)",
    ...) {
  # Parameter validation
  if (is.null(object)) {
    stop("Please provide a valid Seurat object.")
  }

  if (!inherits(object, "Seurat")) {
    stop("object must be a Seurat object.")
  }

  if (is.null(gene) && is.null(markerGene)) {
    stop("Please provide either 'gene' or 'markerGene' parameter.")
  }

  if (!is.null(gene) && !is.null(markerGene)) {
    stop("Please provide only one of 'gene' or 'markerGene' parameters, not both.")
  }

  if (!is.null(assay) && !assay %in% Seurat::Assays(object)) {
    stop(paste0("Assay ", assay, " not found in the Seurat object."))
  }

  if (!id %in% colnames(object@meta.data)) {
    stop(paste0("Column ", id, " not found in object metadata."))
  }

  if (!is.null(split.by) && !split.by %in% colnames(object@meta.data)) {
    stop(paste0("Column ", split.by, " not found in object metadata."))
  }

  if (anno == TRUE && is.null(markerGene)) {
    stop("Please provide 'markerGene' parameter when 'anno' is TRUE.")
  }

  if (anno == TRUE && !aesGroName %in% colnames(markerGene)) {
    stop(paste0("Column ", aesGroName, " not found in markerGene data frame."))
  }
  # set assays
  if (is.null(assay)) {
    assay <- Seurat::DefaultAssay(object = object)
  }
  Seurat::DefaultAssay(object = object) <- assay

  # get gene expression
  if (is.null(gene) && !is.null(markerGene)) {
    geneExp <- Seurat::FetchData(
      object = object,
      vars = unique(markerGene$gene),
      slot = slot
    )
  } else if (!is.null(gene) && is.null(markerGene)) {
    geneExp <- Seurat::FetchData(
      object = object,
      vars = gene,
      slot = slot
    )
  } else {
    stop("Please supply one of 'gene' or 'markerGene', not both")
  }

  # get cluster number or celltype
  # whether split by groups
  if (is.null(split.by)) {
    if (id %in% colnames(object@meta.data)) {
      # using seurat_clusters or Idents
      geneExp$id <- object@meta.data[[id]]
    } else {
      geneExp$id <- Seurat::Idents(object)
    }
  } else {
    # using seurat_clusters or Idents
    if (id %in% colnames(object@meta.data)) {
      geneExp$id <- paste(object@meta.data[[id]],
        " (", object@meta.data[[split.by]], ")",
        sep = ""
      )
    } else {
      geneExp$id <- paste(Seurat::Idents(object),
        " (", object@meta.data[[split.by]], ")",
        sep = ""
      )
    }
  }

  # calculate mean expressions and percent expression
  data.plot <- geneExp %>%
    tidyr::pivot_longer(
      cols = -id,
      names_to = "gene",
      values_to = "expression"
    ) %>%
    dplyr::group_by(id, gene) %>%
    dplyr::summarise(
      avg.exp = mean(expm1(expression)),
      pct.exp = (sum(expression > 0) / dplyr::n()) * 100
    ) %>%
    dplyr::ungroup()

  # scale mean expressions
  if (scale == TRUE) {
    data.plot <- data.plot %>%
      dplyr::group_by(gene) %>%
      dplyr::mutate(
        avg.exp.scaled = if (rescale == TRUE) {
          scales::rescale(scale(avg.exp)[, 1], to = c(rescale.min, rescale.max))
        } else {
          Seurat::MinMax(scale(avg.exp)[, 1], min = col.min, max = col.max)
        }
      ) %>%
      dplyr::ungroup()
  } else {
    # no scale, use log1p transformation
    data.plot <- data.plot %>%
      dplyr::mutate(avg.exp.scaled = log1p(avg.exp))
  }

  if (!is.null(markerGene)) {
    celltype_info <- markerGene

    # add celltype and arrange
    data.plot.res <- data.plot %>%
      dplyr::left_join(
        celltype_info %>% dplyr::select(gene, celltype = !!rlang::sym(aesGroName)),
        by = "gene"
      ) %>%
      dplyr::arrange(celltype)
  } else {
    data.plot.res <- data.plot
  }

  # gene order
  if (is.null(gene.order)) {
    data.plot.res$gene <- factor(data.plot.res$gene, levels = unique(data.plot.res$gene))
  } else {
    data.plot.res$gene <- factor(data.plot.res$gene, levels = unique(gene.order))
  }

  # cluster order
  if (!is.null(cluster.order)) {
    data.plot.res$id <- factor(data.plot.res$id, levels = unique(cluster.order))
  }

  # add group info
  if (!is.null(split.by)) {
    data.plot.res$group <- sapply(strsplit(as.character(data.plot.res$id), split = "\\(|\\)"), "[", 2)
  }

  # ============================================================================
  # plot
  pmain <- ggplot2::ggplot(
    data.plot.res,
    ggplot2::aes(x = gene, y = id)
  ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::theme(
      plot.margin = ggplot2::margin(
        t = plot.margin[1], r = plot.margin[2],
        b = plot.margin[3], l = plot.margin[4],
        unit = "cm"
      ),
      axis.text = ggplot2::element_text(color = "black"),
      legend.direction = "horizontal",
      axis.text.x = ggplot2::element_text(
        angle = x.text.angle,
        hjust = x.text.hjust,
        vjust = x.text.vjust
      ),
      legend.position = legend.position
    )

  # colorbar layer
  colorbar.layer <- ggplot2::guides(
    fill = ggplot2::guide_colorbar(
      title = bar.legendTitle,
      title.position = "top",
      title.hjust = 0.5,
      barwidth = grid::unit(bar.width, "cm"),
      frame.colour = "black",
      frame.linewidth = 0.8,
      ticks.colour = "black"
    )
  )
  # point size layer
  point.layer <- ggplot2::guides(
    size = ggplot2::guide_legend(
      title = point.legendTitle,
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      override.aes = list(
        color = "black",
        fill = "grey50"
      ),
      keywidth = grid::unit(size.width, "cm")
    )
  )

  # change colors
  if (is.null(split.by)) {
    if (length(dot.col) == 2) {
      pmain <- pmain +
        ggplot2::scale_fill_gradient(low = dot.col[1], high = dot.col[2])
    } else {
      pmain <- pmain +
        ggplot2::scale_fill_gradient2(
          low = dot.col[1],
          mid = dot.col[2],
          high = dot.col[3],
          midpoint = midpoint
        )
    }
  } else {
    if (split.by.aesGroup == FALSE) {
      pmain <- pmain +
        ggplot2::scale_fill_manual(values = dot.col)
    } else {
      if (length(dot.col) == 2) {
        pmain <- pmain +
          ggplot2::scale_fill_gradient(low = dot.col[1], high = dot.col[2])
      } else {
        pmain <- pmain +
          ggplot2::scale_fill_gradient2(
            low = dot.col[1],
            mid = dot.col[2],
            high = dot.col[3],
            midpoint = midpoint
          )
      }
    }
  }

  # add point or tile layer
  if (point.geom == TRUE) {
    if (is.null(split.by)) {
      pmain <- pmain +
        ggplot2::geom_point(
          ggplot2::aes(fill = avg.exp.scaled, size = pct.exp),
          color = "black", shape = point.shape
        ) +
        colorbar.layer +
        point.layer +
        ggplot2::scale_size(range = c(dot.min, dot.max))
    } else {
      if (split.by.aesGroup == FALSE) {
        pmain <- pmain +
          ggplot2::geom_point(
            ggplot2::aes(fill = group, size = pct.exp),
            color = "black", shape = point.shape
          ) +
          point.layer +
          ggplot2::scale_size(range = c(dot.min, dot.max))
      } else {
        pmain <- pmain +
          ggplot2::geom_point(
            ggplot2::aes(fill = avg.exp.scaled, size = pct.exp),
            color = "black", shape = point.shape
          ) +
          colorbar.layer +
          point.layer +
          ggplot2::scale_size(range = c(dot.min, dot.max))
      }
    }
  } else if (tile.geom == TRUE) {
    if (is.null(split.by)) {
      pmain <- pmain +
        ggplot2::geom_tile(
          ggplot2::aes(fill = avg.exp.scaled),
          color = "black"
        ) +
        colorbar.layer
    } else {
      if (split.by.aesGroup == FALSE) {
        pmain <- pmain +
          ggplot2::geom_tile(
            ggplot2::aes(fill = group),
            color = "black"
          ) +
          colorbar.layer
      } else {
        pmain <- pmain +
          ggplot2::geom_tile(
            ggplot2::aes(fill = avg.exp.scaled),
            color = "black"
          ) +
          colorbar.layer
      }
    }
  }

  # long to wide matrix
  plot.matrix <- data.plot.res %>%
    tidyr::pivot_wider(names_from = gene, values_from = avg.exp.scaled, id_cols = id)

  rownames(plot.matrix) <- plot.matrix$id
  plot.matrix <- plot.matrix %>% dplyr::select(-id)

  # =======================================
  # define label position
  if (is.null(tree.pos)) {
    if (ytree == TRUE && xtree == FALSE) {
      tree.pos <- c("right", "right")
    } else if (xtree == TRUE && ytree == FALSE) {
      tree.pos <- c("top", "top")
    } else {
      tree.pos <- c("right", "top")
    }
  } else {
    tree.pos <- tree.pos
  }

  # add ytree
  if (ytree == TRUE) {
    # whether put label same with tree
    if (same.pos.label == FALSE) {
      pytree <- pmain +
        ggh4x::scale_y_dendrogram(
          hclust = stats::hclust(stats::dist(plot.matrix)),
          position = tree.pos[1],
          labels = NULL
        ) +
        ggplot2::guides(y.sec = ggh4x::guide_axis_manual(labels = function(x) {
          x
        }))
    } else {
      pytree <- pmain +
        ggh4x::scale_y_dendrogram(
          hclust = stats::hclust(stats::dist(plot.matrix)),
          position = tree.pos[1]
        )
    }
  } else {
    pytree <- pmain
  }

  # add xtree
  if (xtree == TRUE) {
    # whether put label same with tree
    if (same.pos.label == FALSE) {
      pxtree <- pytree +
        ggh4x::scale_x_dendrogram(
          hclust = stats::hclust(stats::dist(t(plot.matrix))),
          position = tree.pos[2],
          labels = NULL
        ) +
        ggplot2::guides(x.sec = ggh4x::guide_axis_manual(labels = function(x) {
          x
        }))
    } else {
      pxtree <- pytree +
        ggh4x::scale_x_dendrogram(
          hclust = stats::hclust(stats::dist(t(plot.matrix))),
          position = tree.pos[2]
        )
    }
  } else {
    pxtree <- pytree
  }

  # ================================
  # add celltype annotation
  if (!is.null(markerGene) && anno == TRUE) {
    panno <- jjAnno::annoSegment(
      object = pxtree,
      annoPos = "top",
      aesGroup = TRUE,
      aesGroName = "celltype",
      segWidth = segWidth,
      lwd = lwd, addBranch = TRUE, branDirection = -1,
      pCol = rep("black", length(unique(celltype_info$cluster))),
      addText = TRUE, textRot = textRot,
      textCol = rep("black", length(unique(celltype_info$cluster))),
      textSize = textSize,
      hjust = hjust,
      ...
    )
  } else {
    panno <- pxtree
  }
  return(panno)
}

###############################
#' This is a test data for this package
#' test data description
#'
#' @name top3pbmc.markers
#' @docType data
#' @author mixfruit
"top3pbmc.markers"
