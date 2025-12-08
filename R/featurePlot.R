#' @name featurePlot
#' @author mixfruit
#' @title  This function creates a scatter plot for multiple genes or features from a
#' Seurat object.
#'
#' @param object A Seurat object containing the data.
#' @param reduction The dimension to use for plotting, default is "umap".
#' @param genes A character vector of gene names or feature names to be plotted.
#' @param nrow Number of rows in the plot grid.
#' @param ncol Number of columns in the plot grid.
#' @param quantile.val The quantile value to determine the color cutoff for each gene.
#' @param color A vector of colors to be used for plotting, defaults to a
#' predefined set of colors.
#' @param rm_axis Logical value indicating whether to remove axis labels and ticks,
#' defaults to FALSE.
#' @param rm_legend Logical value indicating whether to remove the color legend,
#' defaults to FALSE.
#' @param add_rect Logical value indicating whether to add a rectangle around
#' each plot panel, defaults to FALSE.
#' @param add_cor_arrow Logical value indicating whether to add arrows indicating
#' the correlation direction, defaults to FALSE.
#' @param add_strip Logical value indicating whether to add a strip at the top of
#' each plot panel, defaults to FALSE.
#' @param cor_label_dist Distance between the corner arrows and the axis labels.
#' @param arrow_len Length of the corner arrows.
#' @param arrow_label_size Font size of the corner arrow labels.
#' @param plot_size Size of each individual scatter plot.
#' @param keep_one_cor Logical value indicating whether to keep only one set of
#' corner arrows across the entire plot grid, defaults to FALSE.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param respect Logical value indicating whether to respect the specified number
#' of rows and columns in the plot grid, defaults to TRUE.
#' @param point_size the point size, default 1.
#'
#' @examples
#' \dontrun{
#' # Assuming "seurat_obj" is a Seurat object
#' featurePlot(object = seurat_obj, genes = c("gene1", "gene2", "gene3"), nrow = 2, ncol = 2)
#' }
#'
#' @import dplyr
#' @import grDevices
#' @import ggplot2
#' @import Seurat
#' @importFrom stats quantile
#'
#' @export

globalVariables(c("col_rg", "tmp_col", "tmp_color"))

featurePlot <- function(
    object = NULL,
    reduction = "umap",
    genes = NULL,
    nrow = NULL,
    ncol = NULL,
    quantile_val = 1,
    color = NULL,
    rm_axis = FALSE,
    rm_legend = FALSE,
    add_rect = FALSE,
    add_cor_arrow = FALSE,
    add_strip = FALSE,
    cor_label_dist = 0.08,
    arrow_len = 0.2,
    arrow_label_size = 6,
    plot_size = 0.6,
    keep_one_cor = FALSE,
    xlab = NULL,
    ylab = NULL,
    respect = TRUE,
    point_size = 1) {
  # ============================================================================
  # 1_extract data
  # ============================================================================
  # make PC data
  reduc <- data.frame(Seurat::Embeddings(object, reduction = reduction))

  # metadata
  meta <- object@meta.data

  # combine
  pc12 <- cbind(reduc, meta)
  pc12$idents <- Idents(object)

  # get gene expression
  gene_exp <- Seurat::FetchData(object = object, vars = genes)

  # cbind
  mer <- cbind(pc12, gene_exp)

  # get nrow and ncol
  if (is.null(nrow)) {
    nrow <- ifelse(is.null(ncol), 1, ceiling(length(genes) / ncol))
  }

  if (is.null(ncol)) {
    ncol <- ifelse(is.null(nrow), length(genes), ceiling(length(genes) / nrow))
  }

  gene_matrix <- matrix(NA_character_, nrow = nrow, ncol = ncol)
  len <- min(length(genes), nrow * ncol)
  if (len > 0) {
    gene_matrix[seq_len(len)] <- genes[seq_len(len)]
  }

  # assign colors
  if (is.null(color)) {
    cols <- c("grey90", "#57C5B6", "#159895", "#1A5F7A", "#002B5B")
  } else {
    cols <- color
  }

  # ============================================================================
  # 2_draw plot
  # ============================================================================
  lab_shift <- ifelse(rm_axis == FALSE, 
    grid::unit(-2.5, "lines"), 
    grid::unit(-1, "lines")
  )

  # CANVAS FOR PLOT
  grid::grid.newpage()
  grid::pushViewport(
    grid::viewport(
      x = 0.5, y = 0.5,
      width = 0.9, height = 0.9,
      xscale = range(mer[, 1]), yscale = range(mer[, 2]),
      layout = grid::grid.layout(nrow = nrow, ncol = ncol, respect = respect)
    )
  )

  # loop
  for (i in 1:nrow) {
    for (j in 1:ncol) {
      # check genes numbers
      if (i * j > length(genes)) {
        break
      }

      # ===========================================================
      # 1_panel grid
      grid::pushViewport(
        grid::viewport(layout.pos.row = i, layout.pos.col = j)
      )
      if (is.na(gene_matrix[i, j])) { grid::popViewport(); next }

      if (add_rect == TRUE) {
        grid::grid.rect()
      }

      # process data
      quantile_value <- quantile(mer[, gene_matrix[i, j]], probs = quantile_val)
      mer <- mer |> 
        dplyr::mutate(tmp_color = ifelse(.data[[gene_matrix[i, j]]] > quantile_value,
          quantile_value,
          .data[[gene_matrix[i, j]]]
        ))

      tmp_data <- mer |> 
        dplyr::arrange(tmp_color)
      col_palette <- grDevices::colorRampPalette(cols)(100)
      cut_range <- cut(tmp_data[, "tmp_color"], breaks = 100, include.lowest = TRUE)
      col_idx <- as.integer(cut_range)
      tmp_data <- tmp_data |> 
        dplyr::mutate(col_f = col_palette[col_idx])

      # ===========================================================
      # 2_scatter plot
      grid::pushViewport(
        grid::viewport(
          x = 0.5, y = 0.5, width = plot_size, height = plot_size,
          xscale = extendrange(range(tmp_data[, 1]), f = 0.05),
          yscale = extendrange(range(tmp_data[, 2]), f = 0.05)
        )
      )

      # whether add corner arrows
      if (keep_one_cor == TRUE) {
        if (j == 1) {
          if (add_cor_arrow == TRUE) {
            grid::grid.segments(
              x0 = 0, x1 = arrow_len, y0 = 0, y1 = 0,
              arrow = grid::arrow(length = grid::unit(2, "mm"), type = "closed"),
              gp = grid::gpar(fill = "black")
            )
            grid::grid.text(
              label = paste0(toupper(reduction), " 1"), x = arrow_len / 2, y = -cor_label_dist,
              gp = grid::gpar(fontsize = arrow_label_size, fontface = "bold.italic")
            )
            grid::grid.segments(
              x0 = 0, x1 = 0, y0 = 0, y1 = arrow_len,
              arrow = grid::arrow(length = grid::unit(2, "mm"), type = "closed"),
              gp = grid::gpar(fill = "black")
            )
            grid::grid.text(
              label = paste0(toupper(reduction), " 2"),
              x = -cor_label_dist, y = arrow_len / 2, rot = 90,
              gp = grid::gpar(fontsize = arrow_label_size, fontface = "bold.italic")
            )
          } else {
            grid::grid.rect()
          }
        }
      } else {
        if (add_cor_arrow == TRUE) {
          grid::grid.segments(
            x0 = 0, x1 = arrow_len, y0 = 0, y1 = 0,
            arrow = grid::arrow(length = grid::unit(2, "mm"), type = "closed"),
            gp = grid::gpar(fill = "black")
          )
          grid::grid.text(
            label = paste0(toupper(reduction), " 1"), x = arrow_len / 2, y = -cor_label_dist,
            gp = grid::gpar(fontsize = arrow_label_size, fontface = "bold.italic")
          )
          grid::grid.segments(
            x0 = 0, x1 = 0, y0 = 0, y1 = arrow_len,
            arrow = grid::arrow(length = grid::unit(2, "mm"), type = "closed"),
            gp = grid::gpar(fill = "black")
          )
          grid::grid.text(
            label = paste0(toupper(reduction), " 2"),
            x = -cor_label_dist, y = arrow_len / 2, rot = 90,
            gp = grid::gpar(fontsize = arrow_label_size, fontface = "bold.italic")
          )
        } else {
          grid::grid.rect()
        }
      }

      grid::grid.points(
        x = tmp_data[, 1], y = tmp_data[, 2], pch = 19, size = grid::unit(point_size, "pt"),
        gp = grid::gpar(col = tmp_data$col_f)
      )

      # whether draw axis
      if (add_cor_arrow == FALSE) {
        if (rm_axis == FALSE) {
          # grid.xaxis()
          # grid.yaxis()
          grid.xaxis2(label.space = 0.5)
        grid.yaxis2(label.space = 0.25)
        }
      }

      # add strip
      if (add_strip == TRUE) {
        grid::grid.rect(
          x = 0.5, y = 1, width = 1,
          height = 0.15, gp = grid::gpar(fill = "grey85"),
          just = "bottom"
        )
      }

      grid::grid.text(
        label = gene_matrix[i, j], x = 0.5, y = grid::unit(1 + 0.15 / 2, "npc"),
        gp = grid::gpar(fontface = "bold.italic")
      )
      if (add_cor_arrow == FALSE) {
        # axis labels
        axis_label_x <- ifelse(!is.null(xlab), xlab, paste0(toupper(reduction), " dimension 1"))
        axis_label_y <- ifelse(!is.null(ylab), ylab, paste0(toupper(reduction), " dimension 2"))

        grid::grid.text(label = axis_label_x, x = 0.5, y = lab_shift)
        grid::grid.text(label = axis_label_y, x = lab_shift, y = 0.5, rot = 90)
      }

      grid::popViewport()

      # ===========================================================
      # 3_draw legend
      if (rm_legend == FALSE) {
        grid::pushViewport(
          grid::viewport(
            x = 0.5 + plot_size / 2 + 0.01, y = 0.5,
            width = 0.025, height = grid::unit(plot_size, "npc"),
            just = "left",
            yscale = range(tmp_data[, gene_matrix[i, j]])
          )
        )
        # grid.rect(x = 0.5, y = unit(seq(0.25,0.75, length = 100), "npc"),
        #           width = unit(1, "npc"), height = unit(0.5, "npc"),
        #           just = "centre",default.units = "npc",
        #           gp = gpar(col = NA, fill = col_palette))
        # grid.rect(gp = gpar(fill = NA))
        # # grid.yaxis(main = FALSE)
        # jjPlot::grid.yaxis2(side = "right",tick.len = 0.25)

        grid.colorkey(
          x = tmp_data[, gene_matrix[i, j]],
          color = cols,
          pos = "v",
          ticks.side = "right"
        )

        grid::popViewport()
      }
      grid::popViewport()
    }
  }
}
