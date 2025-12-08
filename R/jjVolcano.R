#' @name jjVolcano
#' @author mixfruit
#' @title using jjVolcano to visualize marker genes
#'
#' @param diff_data diff results with data.frame format, default NULL.
#' @param my_markers whether supply your own gene labels, default NULL.
#' @param log2fc_cutoff log2FoldChange cutoff, default 0.25.
#' @param pvalue_cutoff pvalue cutoff to filter, default 0.05.
#' @param adjust_p_cutoff adjusted pvalue cutoff to be colored in plot, default 0.01.
#' @param top_gene_n top genes to be labeled in plot, default 5.
#' @param col_type point color type("updown/adjustP"), default "updown".
#' @param back_col background color, default "grey93".
#' @param p_size point size, default 0.75.
#' @param aes_col point mapping color, default c("#0099CC","#CC3333").
#' @param legend_position legend position in plot, default c(0.7,0.9).
#' @param base_size theme base size, default 14.
#' @param tile_col cluster tile fill color, default useMyCol("paired",n = 9).
#' @param ... other arguments passed by "geom_text_repel".
#' @param cluster_order whether given your cluster orders, default NULL.
#' @param polar whether make the plot to br polar, default FALSE.
#' @param expand the y axis expand, default c(-1,1).
#' @param flip whether flip the plot, default FALSE.
#'
#' @param order_by top marker gene selection method, how the order is, default c("avg_log2FC").
#'
#' @return a ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' jjVolcano(diff_data = pbmc.markers)
#' }
globalVariables(c("p_val", "p_val_adj", "type", "type2"))
jjVolcano <- function(
    diff_data = NULL,
    my_markers = NULL,
    order_by = c("avg_log2FC"), # c("avg_log2FC","p_val")
    log2fc_cutoff = 0.25,
    pvalue_cutoff = 0.05,
    adjust_p_cutoff = 0.01,
    top_gene_n = 5,
    col_type = "updown",
    back_col = "grey93",
    p_size = 0.75,
    aes_col = c("#0099CC", "#CC3333"),
    legend_position = c(0.7, 0.9),
    base_size = 14,
    tile_col = useMyCol("paired", n = 9),
    cluster_order = NULL,
    polar = FALSE,
    expand = c(-1, 1),
    flip = FALSE,
    ...) {
  # filter data
  diff_marker <- diff_data %>%
    dplyr::filter(abs(avg_log2FC) >= log2fc_cutoff & p_val < pvalue_cutoff)

  # assign type
  diff_marker <- diff_marker %>%
    dplyr::mutate(
      type = ifelse(avg_log2FC >= log2fc_cutoff, "sigUp", "sigDown"),
      type2 = ifelse(p_val_adj < adjust_p_cutoff,
        paste("adjust Pvalue < ", adjust_p_cutoff, sep = ""),
        paste("adjust Pvalue >= ", adjust_p_cutoff, sep = "")
      )
    )

  # cluster orders
  if (!is.null(cluster_order)) {
    diff_marker$cluster <- factor(diff_marker$cluster,
      levels = cluster_order
    )
  }

  # get background cols
  back_data <- diff_marker %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(
      min = min(avg_log2FC) - 0.2,
      max = max(avg_log2FC) + 0.2,
      .groups = "drop"
    )

  # get top gene
  top_marker_max <- diff_marker %>%
    dplyr::group_by(cluster) %>%
    dplyr::slice_max(n = top_gene_n, order_by = get(order_by))

  top_marker_min <- diff_marker %>%
    dplyr::group_by(cluster) %>%
    dplyr::slice_min(n = top_gene_n, order_by = get(order_by))

  # combine
  top_marker <- dplyr::bind_rows(top_marker_max, top_marker_min)

  # whether supply own genes
  top_marker <- if (!is.null(my_markers)) {
    diff_marker %>%
      dplyr::filter(gene %in% my_markers)
  } else {
    top_marker
  }

  # ====================================================================
  # plot
  p1 <- ggplot2::ggplot(
    diff_marker,
    ggplot2::aes(x = cluster, y = avg_log2FC)
  ) +
    # add back cols
    ggplot2::geom_col(
      data = back_data,
      ggplot2::aes(x = cluster, y = min), fill = back_col
    ) +
    ggplot2::geom_col(
      data = back_data,
      ggplot2::aes(x = cluster, y = max), fill = back_col
    )

  # ap1 <- paste("adjust Pvalue >= ", adjustP.cutoff, sep = '')
  # ap2 <- paste("adjust Pvalue < ", adjustP.cutoff, sep = '')

  # color type
  p2 <- if (col_type == "updown") {
    p1 +
      # add point
      ggplot2::geom_jitter(ggplot2::aes(color = type), size = p_size) +
      ggplot2::scale_color_manual(values = c("sigDown" = aes_col[1], "sigUp" = aes_col[2]))
  } else if (col_type == "adjustP") {
    p1 +
      ggplot2::geom_jitter(ggplot2::aes(color = type2), size = p_size) +
      ggplot2::scale_color_manual(values = stats::setNames(
        aes_col,
        c(
          paste("adjust Pvalue < ", adjust_p_cutoff, sep = ""),
          paste("adjust Pvalue >= ", adjust_p_cutoff, sep = "")
        )
      ))
  }

  # theme details
  p3 <- p2 +
    ggplot2::scale_y_continuous(n.breaks = 6) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = legend_position,
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank()
    ) +
    ggplot2::xlab("Clusters") + ggplot2::ylab("Average log2FoldChange") +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 5)))

  # add tile
  p4 <- p3 +
    ggplot2::geom_tile(ggplot2::aes(x = cluster, y = 0, fill = cluster),
      color = "black",
      height = log2fc_cutoff * 2,
      alpha = 0.3,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = tile_col) +
    # add gene label
    ggrepel::geom_text_repel(
      data = top_marker,
      ggplot2::aes(x = cluster, y = avg_log2FC, label = gene),
      max.overlaps = 50,
      ...
    )

  # whether coord_plolar
  p5 <- if (polar == TRUE) {
    p4 +
      geomtextpath::geom_textpath(ggplot2::aes(x = cluster, y = 0, label = cluster)) +
      ggplot2::scale_y_continuous(
        n.breaks = 6,
        expand = ggplot2::expansion(mult = expand)
      ) +
      ggplot2::theme_void(base_size = base_size) +
      ggplot2::theme(
        legend.position = legend_position,
        legend.title = ggplot2::element_blank()
      ) +
      ggplot2::coord_polar(clip = "off", theta = "x")
  } else {
    # whether flip plot
    if (flip == TRUE) {
      p4 +
        ggplot2::scale_y_continuous(n.breaks = 6) +
        ggplot2::geom_label(ggplot2::aes(x = cluster, y = 0, label = cluster)) +
        ggplot2::theme(
          axis.line.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        ) +
        ggplot2::coord_flip()
    } else {
      p4 +
        ggplot2::scale_y_continuous(n.breaks = 6) +
        ggplot2::geom_text(ggplot2::aes(x = cluster, y = 0, label = cluster)) +
        ggplot2::theme(
          axis.line.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        )
    }
  }
  return(p5)
}

###############################
#' This is a test data for this package
#' test data description
#'
#' @name pbmc.markers
#' @docType data
#' @author mixfruit
"pbmc.markers"
