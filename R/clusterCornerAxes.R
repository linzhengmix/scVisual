#' @name clusterCornerAxes
#' @author mixfruit
#' @title Add corner axes on seurat UMAP/tSNE cluster figures
#'
#' @param object seurat object.
#' @param reduction "string", reduction type (umap/tsne).
#' @param group_facet "string", give the column name in seurat metadata to facet plot.
#' @param cluster_col "string", the point color to group by,cluster name, default "seurat_clusters".
#' @param p_size "num", point size.
#' @param aspect_ratio "num", plot width and height ratio, default NULL.
#' @param no_split "logic", whether to split/facet the plot, default "TRUE".
#' @param nrow "num", rows to plot when no_split = FALSE.
#' @param rel_length "num", the corner axis line relative length to plot axis(0-1).
#' @param rel_dist "num" ,the relative distance of corner axis label to axis.
#' @param axes "string", show corner axes on all facets, only "mul" is supported now, default "mul".
#' @param legend_pos "string", legend position same as ggplot theme function, default "right".
#' @param key_size The legned point size, default is 5.
#' @param line_text_col "string", corner line and label color, default "black".
#' @param strip_col "string", facet background color, default "white".
#' @param arrow_type "string", arrow type (open/closed), default "closed".
#' @param corner_text_size "num", the corner label text size, default is 3.
#' @param base_size "num", theme base size, default is 14.
#' @param theme_bg Another theme style, default is "default", or "bwCorner".
#' @param add_circle "logic", whether add circle on clusters, default is "FALSE".
#' @param add_circle_legacy "logic", using the legacy version to add a circle, the parameters `nbin`, `nsm`, `addsm`, `sfac` and `qval` are only applicable to legacy, default is "FALSE".
#' @param cic_alpha "num", circle fill color alpha, default is 0.1.
#' @param cic_delta "num", the distance to extend the curve (circle), this parameter only takes effect when `add_circle_legacy = FALSE`.
#' @param cic_line_size "num", circle line size, default is 1.
#' @param cic_line_color "num", circle line color, default is "grey50".
#' @param cic_line_lty "num", circle line type, default is "dashed".
#' @param nbin "num", number of points used to shape the hull, default 100.
#' @param nsm "num", number of points used to perform convolution, should less than nbin, default 10.
#' @param addsm "num", number of additional times of convolution performed, default 1.
#' @param qval "num", expansion size factor, larger value means bigger hull, default 1.5.
#' @param sfac "num", quantile of each sector, used to determine the edge of the hull, should less than 1, default 1.5.
#'
#' @param cell_label Whether to label cell type on plot, default is FALSE.
#' @param cell_label_size Cell type label size, default is 6.
#' @param cell_label_color Cell type label color, default is "black".
#' @param show_legend Whether show legend, default is TRUE.
#'
#' @param corner_variable Which group corner axis to be added when "axes" set to "one", default is the first level.
#'
#' @importFrom ggunchull stat_unchull0
#' @importFrom rlang .data quo

#' @return Return a ggplot object.
#' @export
#' @examples
#' \dontrun{test <- system.file("extdata", "seuratTest.RDS", package = "scVisual")
#'
#' tmp <- readRDS(test)
#'
#' # umap
#' clusterCornerAxes(
#'   object = tmp, reduction = "umap",
#'   no_split = TRUE
#' )
#'
#' # arrowType
#' clusterCornerAxes(
#'   object = tmp, reduction = "umap",
#'   no_split = TRUE, arrow_type = "open"
#' )
#'
#' # facet by metadata column "orig.ident"
#' clusterCornerAxes(
#'   object = tmp, reduction = "umap",
#'   no_split = FALSE, group_facet = "orig.ident",
#'   rel_length = 0.5
#' )
#'
#' # retain only one axes with factor ordering
#' tmp$orig.ident <- factor(tmp$orig.ident, levels = c("ST2", "ST3", "ST1", "ST4"))
#' clusterCornerAxes(
#'   object = tmp, reduction = "umap",
#'   no_split = FALSE, group_facet = "orig.ident",
#'   rel_length = 0.5,
#'   axes = "one"
#' )
#'
#' # line color
#' clusterCornerAxes(
#'   object = tmp, reduction = "umap",
#'   no_split = FALSE, group_facet = "orig.ident",
#'   rel_length = 0.5,
#'   line_text_col = "grey50"
#' )
#'
#' # tsne
#' clusterCornerAxes(
#'   object = tmp, reduction = "tsne",
#'   no_split = FALSE, group_facet = "orig.ident",
#'   rel_length = 0.5
#' )
#' }

# define variables
globalVariables(c("x1", "y1", "linegrou", "angle", "lab", ".data", "corner_variable", "pos_media1", "pos_media2"))

# define function
clusterCornerAxes <- function(
    object = NULL,
    reduction = "umap",
    group_facet = NULL,  # ✅ 修复递归引用问题
    cluster_col = "seurat_clusters",
    p_size = 1,
    aspect_ratio = NULL,
    no_split = TRUE,
    nrow = 1,
    rel_length = 0.25,
    rel_dist = 0.1,
    axes = "mul",
    show.legend = TRUE,
    legend_pos = "right",
    key_size = 5,
    cell_label = FALSE,
    cell_label_size = 6,
    cell_label_color = "black",
    line_text_col = "black",
    strip_col = "white",
    arrow_type = "closed",
    corner_text_size = 3,
    base_size = 14,
    theme_bg = "default",
    add_circle = FALSE,
    add_circle_legacy = FALSE,
    cic_delta = 0.1,
    cic_alpha = 0.1,
    cic_line_size = 1,
    cic_line_color = "grey50",
    cic_line_lty = "dashed",
    nbin = 100,
    nsm = 10,
    addsm = 1,
    qval = 1,
    sfac = 1.5,
    corner_variable = NULL) {

  # make PC data
  reduc <- data.frame(Seurat::Embeddings(object, reduction = reduction))

  # metadata
  meta <- object@meta.data

  # combine
  pc12 <- cbind(reduc, meta)

  # validate factor support
  if (!is.null(group_facet)) {
    validate_factor_support(pc12, group_facet, "clusterCornerAxes")
  }

  #######################################
  # text data
  name_pos <- pc12 %>%
    dplyr::group_by(.data[[cluster_col]]) %>%
    dplyr::summarise(
      pos_media1 = stats::median(get(colnames(pc12)[1])),
      pos_media2 = stats::median(get(colnames(pc12)[2]))
    )

  #######################################

  # data range
  range <- floor(min(min(pc12[, 1]), min(pc12[, 2])))

  # get bottom-left coord
  lower <- range - rel_dist * abs(range)

  # label reldist to axes
  label_rel <- rel_dist * abs(lower)

  # get relative line length
  line_len <- abs(rel_length * lower) + lower

  # mid point
  mid <- abs(rel_length * lower) / 2 + lower

  # give reduction type
  if (startsWith(reduction, "umap")) {
    axs_label <- paste("UMAP", 2:1, sep = "")
  } else if (startsWith(reduction, "tsne")) {
    axs_label <- paste("t-SNE", 2:1, sep = "")
  } else {
    stop("Please give correct type (umap or tsne)!")
  }

  # Handle axes parameter differently based on value
  if (axes == "one" && !no_split && !is.null(group_facet)) {
    # For axes="one", only show axes on the first facet
    
    # Create axes data with only the first facet value
    if (is.factor(pc12[, group_facet])) {
      # If it's a factor, respect the factor levels
      first_level <- levels(pc12[, group_facet])[1]
      
      # Create axes data with the first factor level, preserving factor class and levels
      axes_data <- data.frame(
        "x1" = c(lower, lower, lower, line_len),
        "y1" = c(lower, line_len, lower, lower),
        "linegrou" = c(1, 1, 2, 2),
        "tmp_facet" = factor(rep(first_level, 4), 
                             levels = levels(pc12[, group_facet]))  # Preserve original levels
      )
      
      # Create label data with the first factor level, preserving factor class and levels
      label_data <- data.frame(
        "lab" = c(axs_label),
        "angle" = c(90, 0),
        "x1" = c(lower - label_rel, mid),
        "y1" = c(mid, lower - label_rel),
        "tmp_facet" = factor(rep(first_level, 2), 
                             levels = levels(pc12[, group_facet]))  # Preserve original levels
      )
    } else {
      # Otherwise, use the first unique value as character/numeric
      first_val <- unique(pc12[, group_facet])[1]
      
      # Create axes data with the first value
      axes_data <- data.frame(
        "x1" = c(lower, lower, lower, line_len),
        "y1" = c(lower, line_len, lower, lower),
        "linegrou" = c(1, 1, 2, 2),
        "tmp_facet" = rep(first_val, 4)
      )
      
      # Create label data with the first value
      label_data <- data.frame(
        "lab" = c(axs_label),
        "angle" = c(90, 0),
        "x1" = c(lower - label_rel, mid),
        "y1" = c(mid, lower - label_rel),
        "tmp_facet" = rep(first_val, 2)
      )
    }
    
    # Rename the temporary facet column to match the actual group_facet name
    colnames(axes_data)[4] <- group_facet
    colnames(label_data)[5] <- group_facet
    
  } else {
    # For axes="mul" or no faceting, show axes on all facets
    # Create axes data without facet mapping (will be added automatically)
    axes_data <- data.frame(
      "x1" = c(lower, lower, lower, line_len),
      "y1" = c(lower, line_len, lower, lower),
      "linegrou" = c(1, 1, 2, 2)
    )
    
    label_data <- data.frame(
      "lab" = c(axs_label),
      "angle" = c(90, 0),
      "x1" = c(lower - label_rel, mid),
      "y1" = c(mid, lower - label_rel)
    )
  }

  ######################################################
  # plot
  # Get axis column names
  x_col <- colnames(pc12)[1]
  y_col <- colnames(pc12)[2]
  
  p <- ggplot2::ggplot(
    pc12,
    ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])
  ) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data[[cluster_col]]),
      size = p_size,
      show.legend = show.legend
    ) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(colour = NA, fill = strip_col),
      aspect.ratio = aspect_ratio,
      legend.position = legend_pos,
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank()
    )
  
  # Add axes and labels
  # For axes="one", axes_data and label_data already contain only first facet data
  p <- p +
    ggplot2::geom_line(
      data = axes_data,
      ggplot2::aes(x = x1, y = y1, group = linegrou),
      color = line_text_col,
      arrow = ggplot2::arrow(
        length = grid::unit(0.1, "inches"),
        ends = "last",
        type = arrow_type
      )
    ) +
    ggplot2::geom_text(
      data = label_data,
      ggplot2::aes(x = x1, y = y1, angle = angle, label = lab),
      color = line_text_col,
      fontface = "italic",
      size = corner_text_size
    )
  
  p <- p + ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = key_size)))

  ######################################################
  # add text label
  if (cell_label == FALSE) {
    plabel <- p
  } else {
    plabel <- p +
      ggrepel::geom_text_repel(
        data = name_pos,
        ggplot2::aes(x = pos_media1, y = pos_media2, label = .data[[cluster_col]]),
        show.legend = FALSE,
        size = cell_label_size,
        color = cell_label_color
      )
  }

  ######################################################
  # add circle line
  if (add_circle == FALSE) {
    p0 <- plabel
  } else if (add_circle_legacy) {
    p0 <- plabel +
      ggunchull::stat_unchull0(
        ggplot2::aes(fill = .data[[cluster_col]]),
        alpha = cic_alpha,
        size = cic_line_size,
        color = cic_line_color,
        lty = cic_line_lty,
        show.legend = FALSE,
        nbin = nbin,
        nsm = nsm,
        addsm = addsm,
        sfac = sfac,
        qval = qval
      )
  } else {
    p0 <- plabel +
      ggunchull::stat_unchull(
        ggplot2::aes(fill = .data[[cluster_col]]),
        alpha = cic_alpha,
        size = cic_line_size,
        color = cic_line_color,
        lty = cic_line_lty,
        show.legend = FALSE,
        delta = cic_delta
      )
  }

  ######################################################
  # facet plot
  if (no_split == TRUE) {
    p1 <- p0
  } else {
    p1 <- p0 + ggplot2::facet_wrap(ggplot2::vars(.data[[group_facet]]), nrow = nrow)
  }

  ######################################################
  # theme style
  if (theme_bg == "bwCorner") {
    p2 <- p1 +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        aspect.ratio = 1,
        strip.background = ggplot2::element_rect(colour = NA, fill = strip_col)
      )
  } else if (theme_bg == "default") {
    p2 <- p1
  }

  return(p2)
}
