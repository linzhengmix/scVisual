#' @name featureCornerAxes
#' @author mixfruit
#' @title Add corner axes on seurat UMAP/tSNE gene FeaturePlot function figures
#'
#' @param object object seurat object.
#' @param reduction "string", reduction type (umap/tsne).
#' @param features "string", the gene you want to plot.
#' @param group_facet "string", give the column name in seurat metadata to facet plot, if it is "NULL", facet plot only by gene.
#' @param rel_length "num", the corner axis line relative length to plot axis(0-1).
#' @param rel_dist "num", the relative distance of corner axis label to axis.
#' @param aspect_ratio "num", plot width and height ratio, default NULL.
#' @param low "string", point color with low expression.
#' @param high "string", point color with high expression.
#' @param axes "string", show multiple corner axis or only one (mul/one), default "mul".
#' @param legend_pos "string", legend position same as ggplot theme function, default "right".
#' @param strip_col "string", facet background color, defaults "white".
#' @param p_size "num", point size.
#' @param arrow_type "string", arrow type (open/closed), default "closed".
#' @param line_text_col "string", facet background color, default "white".
#' @param corner_text_size "num", the corner label text size, default is 5.
#' @param base_size "num", theme base size, default is 14.
#' @param theme_bg Another theme style, default is "default", or "bwCorner".
#' @param show.legend Whether show legend, default is "TRUE".
#' @param cornerVariable Which group corner axis to be added when "axes" set to "one", default is the first group.
#' @param n_layout = NULL Similar to the ncol/nrow for the layout, default is the gene numbers.
#' @param min_exp Minimum expression value defined, default is NULL.
#' @param max_exp Maxmum expression value defined, default is NULL.
#' @return Return a ggplot.
#' @export
#' @examples
#' \dontrun{
#'
#' test <- system.file("extdata", "seuratTest.RDS", package = "scVisual")
#'
#' tmp <- readRDS(test)
#'
#' # umap
#' featureCornerAxes(
#'   object = tmp, reduction = "umap",
#'   group_facet = "orig.ident",
#'   rel_length = 0.5, rel_dist = 0.2,
#'   features = c("Actb", "Ythdc1", "Ythdf2")
#' )
#'
#' # one axes with factor ordering
#' tmp$orig.ident <- factor(tmp$orig.ident, levels = c("ST2", "ST3", "ST1", "ST4"))
#' featureCornerAxes(
#'   object = tmp, reduction = "umap",
#'   group_facet = "orig.ident",
#'   features = c("Actb", "Ythdc1", "Ythdf2"),
#'   rel_length = 0.5, rel_dist = 0.2,
#'   axes = "one",
#'   line_text_col = "grey50"
#' )
#'
#' # tsne
#' featureCornerAxes(
#'   object = tmp, reduction = "tsne",
#'   group_facet = "orig.ident",
#'   rel_length = 0.5, rel_dist = 0.2,
#'   features = c("Actb", "Ythdc1", "Ythdf2")
#' )
#'
#' }
# define variables
globalVariables(c("x1", "y1", "linegrou", "angle", "lab", "gene_name", "scaledValue"))

# define function
featureCornerAxes <- function(
    object = NULL,
    reduction = "umap",
    features = NULL,
    groupFacet = "orig.ident",
    minExp = NULL,
    maxExp = NULL,
    relLength = 0.25,
    relDist = 0.1,
    aspectRatio = NULL,
    low = "lightgrey",
    high = "red",
    axes = "mul",
    showLegend = TRUE,
    legendPos = "right",
    stripCol = "white",
    cornerVariable = NULL,
    nLayout = NULL,
    pSize = 1,
    arrowType = "closed",
    lineTextCol = "black",
    cornerTextSize = 3,
    baseSize = 14,
    themeBg = "default") {

  # make PC data
  reduc <- data.frame(Seurat::Embeddings(object, reduction = reduction))

  # metadata
  meta <- object@meta.data

  # combine
  pc12 <- cbind(reduc, meta)

  # validate factor support if groupFacet is provided
  if (!is.null(groupFacet) && groupFacet != "orig.ident") {
    # Check if groupFacet exists in metadata
    if (!groupFacet %in% colnames(pc12)) {
      stop(paste0("Column ", groupFacet, " not found in metadata"))
    }
  }

  # get gene expression
  geneExp <- Seurat::FetchData(object = object, vars = features)

  # cbind
  mer <- cbind(pc12, geneExp)

  # merge data
  megredf <- tidyr::pivot_longer(
    data = mer,
    cols = -dplyr::all_of(colnames(pc12)),
    names_to = "gene_name",
    values_to = "scaledValue"
  )

  # get column names for reduction components
  comp1 <- colnames(reduc)[1]
  comp2 <- colnames(reduc)[2]

  # data range
  range <- floor(min(min(pc12[[comp1]]), min(pc12[[comp2]])))

  # get bottom-left coord
  lower <- range - relDist * abs(range)

  # label reldist to axes
  labelRel <- relDist * abs(lower)

  # get relative line length
  lineLen <- abs(relLength * lower) + lower

  # mid point
  mid <- abs(relLength * lower) / 2 + lower

  # give reduction type
  if (startsWith(reduction, "umap")) {
    axsLabel <- paste("UMAP", 2:1, sep = "")
  } else if (startsWith(reduction, "tsne")) {
    axsLabel <- paste("t-SNE", 2:1, sep = "")
  } else {
    stop("Please give correct type (umap or tsne)!")
  }

  if (axes == "mul") {
    # axises data
    axesData <- data.frame(
      "x1" = c(lower, lower, lower, lineLen),
      "y1" = c(lower, lineLen, lower, lower),
      "linegrou" = c(1, 1, 2, 2)
    )
    # axises label
    labelData <- data.frame(
      "lab" = c(axsLabel),
      "angle" = c(90, 0),
      "x1" = c(lower - labelRel, mid),
      "y1" = c(mid, lower - labelRel)
    )
  } else if (axes == "one") {
    # improved factor support with cleaner logic
    firstFacet <- if (!is.null(cornerVariable)) {
      # use specified cornerVariable
      cornerVariable
    } else {
      # use first level maintaining factor order
      if (!is.null(groupFacet)) {
        get_first_level(pc12[[groupFacet]])
      } else {
        unique(megredf$gene_name)[1]  # fallback to first gene
      }
    }

    # axises data
    axesData <- data.frame(
      "x1" = c(lower, lower, lower, lineLen),
      "y1" = c(lower, lineLen, lower, lower),
      "linegrou" = c(1, 1, 2, 2),
      "group" = rep(firstFacet, 4)
    )
    # axises label
    labelData <- data.frame(
      "lab" = c(axsLabel),
      angle = c(90, 0),
      "x1" = c(lower - labelRel, mid),
      "y1" = c(mid, lower - labelRel),
      "group" = rep(firstFacet, 2)
    )

    # rename group name
    if (!is.null(groupFacet)) {
      colnames(axesData)[4] <- groupFacet
      colnames(labelData)[5] <- groupFacet
    }
  } else {
    stop("Please give correct args (mul or one)!")
  }

  ####################################
  # set color value range
  if (is.null(minExp) && is.null(maxExp)) {
    minexp <- 0
    maxexp <- round(max(megredf$scaledValue) + 1, digits = 0)
  } else {
    minexp <- minExp
    maxexp <- maxExp
  }

  ####################################################
  # plot
  pmain <- ggplot2::ggplot(
    megredf,
    ggplot2::aes(x = .data[[comp1]], y = .data[[comp2]])
  ) +
    ggplot2::geom_point(
      ggplot2::aes(color = scaledValue),
      size = pSize,
      show.legend = showLegend
    ) +
    ggplot2::theme_classic(base_size = baseSize) +
    ggplot2::scale_color_gradient(
      name = "", low = low, high = high,
      limits = c(minexp, maxexp),
      na.value = high
    ) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::geom_line(
      data = axesData,
      ggplot2::aes(x = x1, y = y1, group = linegrou),
      color = lineTextCol,
      arrow = ggplot2::arrow(
        length = grid::unit(0.1, "inches"),
        ends = "last",
        type = arrowType
      )
    ) +
    ggplot2::geom_text(
      data = labelData,
      ggplot2::aes(x = x1, y = y1, angle = angle, label = lab),
      fontface = "italic",
      color = lineTextCol,
      size = cornerTextSize
    ) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(colour = NA, fill = stripCol),
      strip.text = ggplot2::element_text(size = baseSize),
      strip.text.y = ggplot2::element_text(angle = 0),
      aspect.ratio = aspectRatio,
      legend.position = legendPos,
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank()
    )

  ######################################
  # plot layout
  if (is.null(nLayout)) {
    nLayout <- length(features)
  }

  ######################################
  # facet plot
  if (is.null(groupFacet)) {
    p1 <- pmain +
      ggplot2::facet_wrap(facets = "gene_name", ncol = nLayout)
  } else {
    p1 <- pmain +
      ggplot2::facet_grid(rows = ggplot2::vars(gene_name), cols = ggplot2::vars(.data[[groupFacet]]))
  }

  ######################################
  # theme style
  if (themeBg == "bwCorner") {
    p2 <- p1 +
      ggplot2::theme_bw(base_size = baseSize) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        aspect.ratio = 1,
        strip.background = ggplot2::element_rect(colour = NA, fill = stripCol)
      )
  } else if (themeBg == "default") {
    p2 <- p1
  } else {
    p2 <- p1
  }

  # output
  return(p2)
}
