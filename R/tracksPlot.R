#' @name tracks_plot
#' @aliases tracksPlot
#' @author mixfruit
#' @title Generate a track or heatmap plot based on the provided data
#'
#' @param object An optional object containing the data.
#' @param plot_type The type of plot to generate, either "track" or "heatmap".
#' @param genes A vector or data frame specifying the genes to plot.
#' @param vmin The minimum value for the color scale (only applicable for heatmap).
#' @param vmax The maximum value for the color scale (only applicable for heatmap).
#' @param cell_order An optional vector specifying the order of cells in the plot.
#' @param gene_order An optional vector specifying the order of genes in the plot.
#' @param facet_nested_params A list of additional parameters to customize the facet_nested plot.
#' @param theme_params A list of additional parameters to customize the plot's theme.
#' @param strip_nested_params A list of additional parameters to customize the strip_nested plot.
#'
#' @return A ggplot object representing the track or heatmap plot.
#'
#' @export
#' @importFrom utils modifyList

# Define global variables
utils::globalVariables(c(
  "barcode", "cell", "cluster", 
  "exp", "gene"
))

#' @name tracks_plot
#' @aliases tracksPlot
#' @export
tracks_plot <- function(
    object = NULL,
    plot_type = c("track", "heatmap"),
    genes = NULL,
    vmin = -2, 
    vmax = 2,
    cell_order = NULL,
    gene_order = NULL,
    facet_nested_params = list(),
    theme_params = list(),
    strip_nested_params = list()) {
  plot_type <- match.arg(plot_type, c("track", "heatmap"))

  # check markers gene
  markers <- if (is.data.frame(genes)) {
    markers_tmp <- genes |> 
      dplyr::mutate(gene = make.unique(gene))
    markers <- markers_tmp$gene
    names(markers) <- markers_tmp$cluster
    markers
  } else {
    genes
  }

  # get barcode info
  barcode_info <- data.frame(Seurat::Idents(object))
  barcode_info$barcode <- rownames(barcode_info)
  colnames(barcode_info)[1] <- "cell"

  # get normalized matrix
  df <- object@assays$RNA@data[markers, ] |> 
    t() |> 
    as.data.frame(check.names = FALSE)

  # do zscore
  if (plot_type == "heatmap") {
    df <- df |> 
      dplyr::mutate(dplyr::across(
        dplyr::everything(), 
        ~ scale(.)[, 1]
      )) |> 
      dplyr::mutate(dplyr::across(
        dplyr::everything(), 
        ~ ifelse(. > vmax, vmax, ifelse(. < vmin, vmin, .))
      ))
  }

  df <- df |> 
    dplyr::mutate(barcode = rownames(df))

  # add cell type
  df <- df |> 
    dplyr::left_join(y = barcode_info, by = "barcode")

  # wide to long
  df_long <- tidyr::pivot_longer(
    data = df,
    cols = -dplyr::all_of(c("cell", "barcode")),
    names_to = "gene",
    values_to = "exp"
  )

  # order
  if (!is.null(cell_order)) {
    df_long$cell <- factor(df_long$cell, levels = cell_order)
  }

  if (!is.null(gene_order)) {
    df_long$gene <- factor(df_long$gene, levels = gene_order)
  }

  # whether add cluster
  if (is.data.frame(genes)) {
    df_long <- df_long |> 
      dplyr::left_join(
        y = markers_tmp[, c("gene", "cluster")],
        by = c("gene" = "gene")
      )
  }

  # ============================================================================
  # plot
  # ============================================================================
  # strip color
  strip <- do.call(ggh4x::strip_nested, modifyList(
    list(),
    strip_nested_params
  ))

  # facet layer
  if (is.data.frame(genes)) {
    facet_nested <- do.call(
      ggh4x::facet_nested, modifyList(
        list(
          cluster + gene ~ cell,
          scales = "free",
          space = "fixed",
          switch = "y",
          nest_line = ggplot2::element_line(),
          strip = strip
        ),
        facet_nested_params
      )
    )
  } else {
    facet_nested <- do.call(
      ggh4x::facet_nested, modifyList(
        list(
          gene ~ cell,
          scales = "free",
          space = "fixed",
          switch = "y",
          nest_line = ggplot2::element_line(),
          strip = strip
        ),
        facet_nested_params
      )
    )
  }

  # main layer
  pmain <- ggplot2::ggplot(df_long) +
    ggplot2::theme_bw(base_size = 12) +
    facet_nested +
    do.call(
      ggplot2::theme, modifyList(
        list(
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          strip.placement = "outside",
          strip.background.y = ggplot2::element_blank(),
          strip.text = ggplot2::element_text(face = "bold.italic"),
          strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1)
        ),
        theme_params
      )
    ) +
    ggplot2::labs(x = "", y = "")

  # add layers
  p <- if (plot_type == "heatmap") {
    pmain +
      ggplot2::geom_tile(ggplot2::aes(x = barcode, y = gene, fill = exp)) +
      ggplot2::coord_cartesian(expand = 0) +
      ggplot2::scale_fill_gradient2(
        low = "#313695", 
        mid = "white", 
        high = "#A50026",
        midpoint = 0, 
        na.value = "white"
      )
  } else {
    pmain +
      ggplot2::geom_col(
        ggplot2::aes(x = barcode, y = exp, fill = gene), 
        width = 1
      )
  }

  p
}
