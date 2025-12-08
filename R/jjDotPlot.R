#' @name jjDotPlot
#' @author mixfruit

#' @title Dot Plot for Gene Expression Visualization
#' @param object Seurat object containing the data.
#' @param gene Genes to plot.
#' @param markerGene Marker genes with cell type info.
#' @param ... Additional parameters.
#' @return A ggplot object.
#' @export
#'
#' @description Create a dot plot of average expression and percent expressing
#' across clusters or groups.
#'


# Define global variables
globalVariables(c("%||%", ".", "avg_exp", "avg_exp_scaled", "celltype", "group", "pct_exp", "unit", "data.plot.res"))

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
  assay <- assay %||% Seurat::DefaultAssay(object = object)
  Seurat::DefaultAssay(object = object) <- assay

  # get gene expression
  gene_exp <- if (is.null(gene) && !is.null(markerGene)) {
    Seurat::FetchData(
      object = object,
      vars = unique(markerGene$gene),
      slot = slot
    )
  } else if (!is.null(gene) && is.null(markerGene)) {
    Seurat::FetchData(
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
    gene_exp$id <- if (id %in% colnames(object@meta.data)) {
      object@meta.data[[id]]
    } else {
      Seurat::Idents(object)
    }
  } else {
    id_values <- if (id %in% colnames(object@meta.data)) {
      object@meta.data[[id]]
    } else {
      Seurat::Idents(object)
    }
    gene_exp$id <- paste(id_values, " (", object@meta.data[[split.by]], ")", sep = "")
  }

  # calculate mean expressions and percent expression
  data_plot <- gene_exp %>% 
    tidyr::pivot_longer(
      cols = -id,
      names_to = "gene",
      values_to = "expression"
    ) %>% 
    dplyr::group_by(id, gene) %>% 
    dplyr::summarise(
      avg_exp = mean(expm1(expression)),
      pct_exp = (sum(expression > 0) / dplyr::n()) * 100
    ) %>% 
    dplyr::ungroup()

  # scale mean expressions
  data_plot <- if (scale == TRUE) {
    data_plot %>% 
      dplyr::group_by(gene) %>% 
      dplyr::mutate(
        avg_exp_scaled = if (rescale == TRUE) {
          scales::rescale(scale(avg_exp)[, 1], to = c(rescale.min, rescale.max))
        } else {
          Seurat::MinMax(scale(avg_exp)[, 1], min = col.min, max = col.max)
        }
      ) %>% 
      dplyr::ungroup()
  } else {
    # no scale, use log1p transformation
    data_plot %>% 
      dplyr::mutate(avg_exp_scaled = log1p(avg_exp))
  }

  # add celltype info if markerGene is provided
  data_plot_res <- if (!is.null(markerGene)) {
    celltype_info <- markerGene

    data_plot %>% 
      dplyr::left_join(
        celltype_info %>% dplyr::select(gene, celltype = !!rlang::sym(aesGroName)),
        by = "gene"
      ) %>% 
      dplyr::arrange(celltype)
  } else {
    data_plot
  }

  # gene order
  data_plot_res$gene <- factor(
    data_plot_res$gene,
    levels = if (is.null(gene.order)) {
      unique(data_plot_res$gene)
    } else {
      unique(gene.order)
    }
  )

  # cluster order
  if (!is.null(cluster.order)) {
    data_plot_res$id <- factor(data.plot.res$id, levels = unique(cluster.order))
  }

  # add group info
  if (!is.null(split.by)) {
    data_plot_res$group <- sapply(strsplit(as.character(data_plot_res$id), split = "\\(|\\)"), function(x) x[2])
  }

  # ============================================================================
  # plot
  pmain <- ggplot2::ggplot(
    data_plot_res,
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
  pmain <- if (is.null(split.by)) {
    if (length(dot.col) == 2) {
      pmain + ggplot2::scale_fill_gradient(low = dot.col[1], high = dot.col[2])
    } else {
      pmain + ggplot2::scale_fill_gradient2(
        low = dot.col[1],
        mid = dot.col[2],
        high = dot.col[3],
        midpoint = midpoint
      )
    }
  } else {
    if (split.by.aesGroup == FALSE) {
      pmain + ggplot2::scale_fill_manual(values = dot.col)
    } else {
      if (length(dot.col) == 2) {
        pmain + ggplot2::scale_fill_gradient(low = dot.col[1], high = dot.col[2])
      } else {
        pmain + ggplot2::scale_fill_gradient2(
          low = dot.col[1],
          mid = dot.col[2],
          high = dot.col[3],
          midpoint = midpoint
        )
      }
    }
  }

  # add point or tile layer
  pmain <- if (point.geom == TRUE) {
    if (is.null(split.by)) {
      pmain +
        ggplot2::geom_point(
          ggplot2::aes(fill = avg_exp_scaled, size = pct_exp),
          color = "black", shape = point.shape
        ) +
        colorbar.layer +
        point.layer +
        ggplot2::scale_size(range = c(dot.min, dot.max))
    } else {
      if (split.by.aesGroup == FALSE) {
        pmain +
          ggplot2::geom_point(
            ggplot2::aes(fill = group, size = pct_exp),
            color = "black", shape = point.shape
          ) +
          point.layer +
          ggplot2::scale_size(range = c(dot.min, dot.max))
      } else {
        pmain +
          ggplot2::geom_point(
            ggplot2::aes(fill = avg_exp_scaled, size = pct_exp),
            color = "black", shape = point.shape
          ) +
          colorbar.layer +
          point.layer +
          ggplot2::scale_size(range = c(dot.min, dot.max))
      }
    }
  } else if (tile.geom == TRUE) {
    if (is.null(split.by)) {
      pmain +
        ggplot2::geom_tile(
          ggplot2::aes(fill = avg_exp_scaled),
          color = "black"
        ) +
        colorbar.layer
    } else {
      if (split.by.aesGroup == FALSE) {
        pmain +
          ggplot2::geom_tile(
            ggplot2::aes(fill = group),
            color = "black"
          ) +
          colorbar.layer
      } else {
        pmain +
          ggplot2::geom_tile(
            ggplot2::aes(fill = avg_exp_scaled),
            color = "black"
          ) +
          colorbar.layer
      }
    }
  } else {
    pmain
  }

  # long to wide matrix
  plot_matrix <- data_plot_res %>% 
    tidyr::pivot_wider(names_from = gene, values_from = avg_exp_scaled, id_cols = id)

  rownames(plot_matrix) <- plot_matrix$id
  plot_matrix <- plot_matrix %>% dplyr::select(-id)

  # =======================================
  # define label position
  tree_pos <- if (is.null(tree.pos)) {
    if (ytree == TRUE && xtree == FALSE) {
      c("right", "right")
    } else if (xtree == TRUE && ytree == FALSE) {
      c("top", "top")
    } else {
      c("right", "top")
    }
  } else {
    tree.pos
  }

  # add ytree
  pytree <- if (ytree == TRUE) {
    # whether put label same with tree
    if (same.pos.label == FALSE) {
      pmain +
        ggh4x::scale_y_dendrogram(
          hclust = stats::hclust(stats::dist(plot_matrix)),
          position = tree_pos[1],
          labels = NULL
        ) +
        ggplot2::guides(y.sec = ggh4x::guide_axis_manual(labels = function(x) x))
    } else {
      pmain +
        ggh4x::scale_y_dendrogram(
          hclust = stats::hclust(stats::dist(plot_matrix)),
          position = tree_pos[1]
        )
    }
  } else {
    pmain
  }

  # add xtree
  pxtree <- if (xtree == TRUE) {
    # whether put label same with tree
    if (same.pos.label == FALSE) {
      pytree +
        ggh4x::scale_x_dendrogram(
          hclust = stats::hclust(stats::dist(t(plot_matrix))),
          position = tree_pos[2],
          labels = NULL
        ) +
        ggplot2::guides(x.sec = ggh4x::guide_axis_manual(labels = function(x) x))
    } else {
      pytree +
        ggh4x::scale_x_dendrogram(
          hclust = stats::hclust(stats::dist(t(plot_matrix))),
          position = tree_pos[2]
        )
    }
  } else {
    pytree
  }

  # ================================
  # add celltype annotation
  # Simplified version without jjAnno dependency
  panno <- pxtree
  
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
