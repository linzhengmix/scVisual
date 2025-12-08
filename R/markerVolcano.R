#' @name marker_volcano
#' @aliases markerVolcano
#' @author mixfruit
#' @title Marker genes volcano plot
#'
#' @param markers Dataframe marker genes from findAllmarkers function from seurat.
#' @param own_gene Your own gene names to be labeled on plot, defaults is null.
#' @param top_n Numbers top genes to label, defaults is 5.
#' @param log2fc The threshold of log2FC, defaults is 0.25.
#' @param hline_size Hline size, defaults is 1.
#' @param hline_color Hline color, defaults is "grey50".
#' @param p_force Positive gene force parameters to avoid overlap gene labels,
#' defaults is 5.
#' @param n_force Negative gene force parameters to avoid overlap gene labels,
#' defaults is 2.5.
#' @param nudge_x Adjustments on the horizontal of the gene label, defaults is 0.8.
#' @param p_nudge_y Adjustments on the horizontal of the positive gene label,
#' defaults is 0.25.
#' @param n_nudge_y Adjustments on the horizontal of the negative gene label,
#' defaults is 0.
#' @param base_size Theme base size, defaults is 14.
#' @param facet_color Facet border color, defaults is NA.
#' @param facet_fill Facet fill color, defaults is "white".
#' @param ylab Plot y label, defaults is "Log2-Fold Change".
#' @param nrow Numbers rows to plot, defaults is 1.
#'
#' @return Return a ggplot.
#' @export
#'
#' @examples
#' test <- system.file("extdata", "pbmc.markers.csv", package = "scVisual")
#' markers <- read.csv(test)
#'
#' marker_volcano(
#'   markers = markers,
#'   top_n = 5,
#'   label_col = ggsci::pal_npg()(9)
#' )
# define variables
utils::globalVariables(c("avg_log2FC", "cluster", "gene", "pct.1", "pct.2"))

# define function
marker_volcano <- function(
    markers = NULL,
    own_gene = NULL,
    top_n = 5,
    log2fc = 0.25,
    label_col = NULL,
    hline_size = 1,
    hline_color = "grey50",
    p_force = 5,
    n_force = 2.5,
    nudge_x = 0.8,
    p_nudge_y = 0.25,
    n_nudge_y = 0,
    base_size = 14,
    facet_color = NA,
    facet_fill = "white",
    ylab = "Log2-Fold Change",
    nrow = 1) {
  # whether supply own gene names
  top_gene <- if (is.null(own_gene)) {
    # top genes
    top_pos <- markers |> 
      dplyr::group_by(cluster) |> 
      dplyr::slice_max(n = top_n, order_by = avg_log2FC)
    top_neg <- markers |> 
      dplyr::group_by(cluster) |> 
      dplyr::slice_min(n = top_n, order_by = avg_log2FC)

    # merge
    dplyr::bind_rows(top_pos, top_neg)
  } else {
    markers |> dplyr::filter(gene %in% own_gene)
  }

  # separate positive and negative genes
  top_pos <- top_gene |> dplyr::filter(avg_log2FC > 0)
  top_neg <- top_gene |> dplyr::filter(avg_log2FC < 0)

  # plot
  ggplot2::ggplot(
    markers,
    ggplot2::aes(x = pct.1 - pct.2, y = avg_log2FC)
  ) +
    ggplot2::geom_point(color = "grey80") +
    ggplot2::geom_hline(
      yintercept = c(-log2fc, log2fc),
      lty = "dashed",
      size = hline_size,
      color = hline_color
    ) +
    ggrepel::geom_text_repel(
      data = top_pos,
      ggplot2::aes(
        x = pct.1 - pct.2,
        y = avg_log2FC,
        label = gene,
        color = cluster
      ),
      show.legend = FALSE,
      direction = "y",
      hjust = 1,
      nudge_y = p_nudge_y,
      force = p_force,
      nudge_x = -nudge_x - (top_pos$pct.1 - top_pos$pct.2)
    ) +
    ggrepel::geom_text_repel(
      data = top_neg,
      ggplot2::aes(
        x = pct.1 - pct.2,
        y = avg_log2FC,
        label = gene,
        color = cluster
      ),
      show.legend = FALSE,
      direction = "y",
      hjust = 0,
      nudge_y = n_nudge_y,
      force = n_force,
      nudge_x = nudge_x - (top_neg$pct.1 - top_neg$pct.2)
    ) +
    ggplot2::geom_point(
      data = top_gene,
      show.legend = FALSE,
      ggplot2::aes(
        x = pct.1 - pct.2,
        y = avg_log2FC,
        color = cluster
      )
    ) +
    ggplot2::scale_color_manual(name = "", values = label_col) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      strip.background = ggplot2::element_rect(
        color = facet_color, 
        fill = facet_fill
      )
    ) +
    ggplot2::xlab(expression(Delta ~ "Percentage Difference")) +
    ggplot2::ylab(ylab) +
    ggplot2::facet_wrap(~cluster, nrow = nrow, scales = "fixed")
}
