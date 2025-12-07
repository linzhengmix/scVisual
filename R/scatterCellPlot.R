#' @name scatterCellPlot
#' @author Jun Zhang
#' @title This function creates a scatter cell plot using the grid package in R.
#'
#' @param object A Seurat object containing the data.
#' @param color A vector of colors for each cell type. If NULL, random colors will be assigned.
#' @param dim The dimension used for plotting (default is "umap").
#' @param rm.axis Logical value indicating whether to remove the x and y-axis (default is FALSE).
#' @param cell.id Name of the column in the metadata that represents cell identity (default is NULL).
#' @param bar.width Width of the barplot (default is 0.2).
#' @param point.size Size of the points in the scatter plot (default is 1).
#' @param rm.barplot whether remove barplot, default FALSE.
#' @param legend.psize legend point size, default 1.5.
#' @param arrow.len arrow length, default 0.2.
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' scatterCellPlot(object = seurat_object)
#' }
#'
#'
#' @export

globalVariables(c("idents"))

scatterCellPlot <- function(
    object = NULL,
    color = NULL,
    dim = "umap",
    rm.axis = FALSE,
    cell.id = NULL,
    bar.width = 0.2,
    point.size = 1,
    rm.barplot = FALSE,
    legend.psize = 1.5,
    arrow.len = 0.2) {
  # ============================================================================
  # 1_extract data
  # ============================================================================
  # make PC data
  reduc <- data.frame(Seurat::Embeddings(object, reduction = dim))

  # metadata
  meta <- object@meta.data

  # combine
  pc12 <- cbind(reduc, meta)
  pc12$idents <- as.character(Seurat::Idents(object))

  # summary celltype numbers
  if (is.null(cell.id)) {
    cell_num <- pc12 |>
      dplyr::group_by(idents) |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::arrange(n)
  } else {
    cell_num <- pc12 |>
      dplyr::group_by(idents, .data[[cell.id]]) |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::arrange(n)
  }

  # ============================================================================
  # 2_draw plot
  # ============================================================================
  if (rm.axis == FALSE) {
    lab.shift <- grid::unit(-2.5, "lines")
  } else {
    lab.shift <- grid::unit(-1, "lines")
  }

  grid::grid.newpage()
  grid::pushViewport(
    grid::viewport(
      x = grid::unit(0.1, "npc"), y = grid::unit(0.5, "npc"),
      width = grid::unit(0.5, "npc"),
      height = grid::unit(0.7, "npc"),
      just = "left",
      xscale = grDevices::extendrange(range(pc12[, 1]), f = 0.05),
      yscale = grDevices::extendrange(range(pc12[, 2]), f = 0.05),
    )
  )
  grid::grid.rect()
  if (rm.axis == FALSE) {
    # grid.xaxis()
    # grid.yaxis()
    jjPlot::grid.xaxis2(label.space = 0.5)
    jjPlot::grid.yaxis2(label.space = 0.25)
  }

  celltype <- cell_num$idents

  if (is.null(color)) {
    # create colors
    cols <- circlize::rand_color(n = length(celltype))
  } else {
    cols <- color
  }

  # draw points
  # i = 1
  for (i in seq_along(celltype)) {
    # tmp <- pc12 |>
    #   dplyr::filter(idents == celltype[i])
    tmp <- pc12[which(pc12$idents %in% celltype[i]), ]

    grid::grid.points(
      x = tmp[, 1], y = tmp[, 2], pch = 19, size = grid::unit(point.size, "pt"),
      gp = grid::gpar(col = cols[i])
    )
  }

  # arrow
  if (rm.axis == TRUE) {
    grid::grid.segments(
      x0 = 0.025, x1 = arrow.len, y0 = 0.05, y1 = 0.05,
      arrow = grid::arrow(length = grid::unit(2, "mm"), type = "closed"),
      gp = grid::gpar(fill = "black")
    )
    grid::grid.text(
      label = paste0(toupper(dim), " 1"),
      x = (arrow.len + 0.025) / 2, y = 0.025,
      gp = grid::gpar(fontsize = 6, fontface = "bold.italic")
    )
    grid::grid.segments(
      x0 = 0.05, x1 = 0.05, y0 = 0.025, y1 = arrow.len,
      arrow = grid::arrow(length = grid::unit(2, "mm"), type = "closed"),
      gp = grid::gpar(fill = "black")
    )
    grid::grid.text(
      label = paste0(toupper(dim), " 2"),
      x = 0.025, y = (arrow.len + 0.025) / 2, rot = 90,
      gp = grid::gpar(fontsize = 6, fontface = "bold.italic")
    )
  } else {
    # labs
    grid::grid.text(label = paste0(toupper(dim), " dimension 1"), x = 0.5, y = lab.shift)
    grid::grid.text(label = paste0(toupper(dim), " dimension 2"), x = lab.shift, y = 0.5, rot = 90)
  }

  grid::popViewport()

  # ============================================================================
  # barplot
  if (isFALSE(rm.barplot)) {
    grid::pushViewport(
      grid::viewport(
        x = grid::unit(0.61, "npc"), y = grid::unit(0.5, "npc"),
        width = grid::unit(bar.width, "npc"),
        height = grid::unit(0.7, "npc"),
        just = "left",
        yscale = c(0, nrow(cell_num) + 0.75),
        xscale = c(0, max(cell_num$n) + 0.1 * max(cell_num$n))
      )
    )

    if (rm.axis == FALSE) {
      # grid.xaxis()
      jjPlot::grid.xaxis2(
        label.space = 0.5,
        at = c(0, max(cell_num$n)),
        labels = as.character(c(0, max(cell_num$n)))
      )
    }
    grid::grid.rect(
      x = rep(0, nrow(cell_num)), y = seq_len(nrow(cell_num)),
      width = cell_num$n, height = grid::unit(0.08, "npc"),
      just = "left",
      gp = grid::gpar(fill = cols, col = NA),
      default.units = "native"
    )
    grid::grid.rect(gp = grid::gpar(fill = "transparent"))
    grid::grid.text(label = "Number of cells", x = 0.5, y = lab.shift)
    grid::popViewport()
  }

  # ============================================================================
  # legend
  if (isTRUE(rm.barplot)) {
    bar.width <- 0
  }

  grid::pushViewport(
    grid::viewport(
      x = grid::unit(0.61 + bar.width, "npc"), y = grid::unit(0.5, "npc"),
      width = grid::unit(0.2, "npc"),
      height = grid::unit(0.7, "npc"),
      just = "left",
      yscale = c(0, nrow(cell_num) + 0.75)
    )
  )

  grid::grid.points(
    x = rep(0.1, nrow(cell_num)), y = seq_len(nrow(cell_num)), pch = 19,
    gp = grid::gpar(col = cols), size = grid::unit(legend.psize, "char")
  )
  if (!is.null(cell.id)) {
    grid::grid.text(
      label = as.character(unlist(cell_num[, cell.id])),
      x = 0.1, y = seq_len(nrow(cell_num)),
      default.units = "native"
    )
  }
  grid::grid.text(
    label = cell_num$idents,
    x = 0.2, y = seq_len(nrow(cell_num)),
    just = "left",
    gp = grid::gpar(fontsize = 10),
    default.units = "native"
  )
  # grid.rect(gp = gpar(fill = "transparent"))
  grid::popViewport()
}
