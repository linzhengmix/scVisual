#' @name drawLegend
#' @author mixfruit
#' @title using drawLegend to add cluster id in legend
#'
#' @param object object seurat object, default NULL.
#' @param plot ggplot object, default NULL.
#' @param cell_type cell type column name, default NULL.
#' @param clusters cluster id column name, default NULL.
#' @param ncol ncols to draw legend(1/2), default 1.
#' @param col point color, default hue_pal().
#' @param pt.size point size, default 8.
#' @param text.size text size, default 4.
#'
#' @return combine plot
#' @export

globalVariables(c("x", "y", "leg.data"))

drawLegend <- function(
    object = NULL,
    plot = NULL,
    cell_type = NULL,
    clusters = NULL,
    ncol = 1,
    col = NULL,
    pt.size = 8,
    text.size = 4) {
  # prepare data
  leg_data <- object@meta.data %>%
    dplyr::select(.data[[cell_type]], .data[[clusters]]) %>%
    unique()

  colnames(leg_data) <- c("cell_type", "clusters")

  # reorder
  leg_data$cell_type <- factor(leg_data$cell_type)
  leg_data <- leg_data[match(levels(leg_data$cell_type), leg_data$cell_type), ]

  # add xy position
  if (ncol > 1) {
    leg_data$x <- rep(
      1:ncol,
      c(
        ceiling(nrow(leg_data) / ncol),
        nrow(leg_data) - ceiling(nrow(leg_data) / ncol)
      )
    )

    leg_data$y <- c(
      1:ceiling(nrow(leg_data) / ncol),
      1:(nrow(leg_data) - ceiling(nrow(leg_data) / ncol))
    )
  } else {
    leg_data$x <- 1
    leg_data$y <- seq_len(nrow(leg_data))
  }

  # order
  leg_data$cell_type <- factor(leg_data$cell_type, levels = rev(levels(leg_data$cell_type)))

  # plot
  if (is.null(col)) {
    color <- rev(scales::hue_pal()(nrow(leg.data)))
  } else {
    color <- rev(col)
  }

  pleg <- ggplot2::ggplot(leg_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(
      ggplot2::aes(color = cell_type),
      show.legend = FALSE,
      size = pt.size
    ) +
    ggplot2::geom_text(ggplot2::aes(label = clusters)) +
    ggplot2::geom_text(
      ggplot2::aes(label = cell_type),
      hjust = 0,
      nudge_x = 0.2,
      size = text.size
    ) +
    ggplot2::scale_color_manual(values = color) +
    ggplot2::scale_y_reverse() +
    ggplot2::xlim(0, ncol + 1) +
    ggplot2::theme_void()

  # COMBINE
  cowplot::plot_grid(plot, pleg)
}
