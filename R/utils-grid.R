#' @name grid.xaxis2
#' @author mixfruit
#' @title Custom x-axis for grid plots
#' @description Create a custom x-axis for grid plots
#' @param at Position of ticks
#' @param breaks Number of breaks
#' @param labels Tick labels
#' @param tick.len Tick length
#' @param label.space Space between ticks and labels
#' @param side Side of the axis (bottom or top)
#' @param rot Label rotation
#' @param label.size Label font size
#' @return None
#' @export
grid.xaxis2 <- function(
  at = NULL, 
  breaks = 5, 
  labels = NULL, 
  tick.len = 0.5, 
  label.space = 0.5, 
  side = c("bottom", "top"), 
  rot = 0, 
  label.size = 12
) {
  if (is.null(at) || is.null(labels)) {
    at <- grid::grid.pretty(grid::current.viewport()$xscale, n = breaks)
    labels <- as.character(at)
  } else {
    at <- at
    labels <- as.character(labels)
  }
  
  side <- match.arg(side, c("bottom", "top"))
  
  if (side == "bottom") {
    tck.y0 <- grid::unit(0, "npc")
    tck.y1 <- grid::unit(-tick.len, "lines")
    text.y <- grid::unit(-tick.len - label.space, "lines")
  } else {
    tck.y0 <- grid::unit(1, "npc")
    tck.y1 <- grid::unit(1, "npc") + grid::unit(tick.len, "lines")
    text.y <- grid::unit(abs(tick.len) + abs(label.space), "lines") + grid::unit(1, "npc")
  }
  
  grid::grid.segments(x0 = 0, x1 = 1, y0 = 0, y1 = 0)
  grid::grid.segments(
    x0 = grid::unit(at, "native"), 
    x1 = grid::unit(at, "native"), 
    y0 = tck.y0, 
    y1 = tck.y1
  )
  grid::grid.text(
    label = labels, 
    x = grid::unit(at, "native"), 
    y = text.y, 
    rot = rot, 
    gp = grid::gpar(fontsize = label.size)
  )
}

#' @name grid.yaxis2
#' @author mixfruit
#' @title Custom y-axis for grid plots
#' @description Create a custom y-axis for grid plots
#' @param at Position of ticks
#' @param breaks Number of breaks
#' @param labels Tick labels
#' @param tick.len Tick length
#' @param label.space Space between ticks and labels
#' @param side Side of the axis (left or right)
#' @param rot Label rotation
#' @param label.size Label font size
#' @return None
#' @export
grid.yaxis2 <- function(
  at = NULL, 
  breaks = 5, 
  labels = NULL, 
  tick.len = 0.5, 
  label.space = 0.25, 
  side = c("left", "right"), 
  rot = 0, 
  label.size = 12
) {
  if (is.null(at) || is.null(labels)) {
    at <- grid::grid.pretty(grid::current.viewport()$yscale, n = breaks)
    labels <- as.character(at)
  } else {
    at <- at
    labels <- as.character(labels)
  }
  
  side <- match.arg(side, c("left", "right"))
  
  if (side == "left") {
    tck.x0 <- grid::unit(0, "npc")
    tck.x1 <- grid::unit(-tick.len, "lines")
    text.x <- grid::unit(-tick.len - label.space, "lines")
    text.just <- "right"
  } else {
    tck.x0 <- grid::unit(1, "npc")
    tck.x1 <- grid::unit(1, "npc") + grid::unit(tick.len, "lines")
    text.x <- grid::unit(abs(tick.len) + abs(label.space), "lines") + grid::unit(1, "npc")
    text.just <- "left"
  }
  
  grid::grid.segments(x0 = 0, x1 = 0, y0 = 0, y1 = 1)
  grid::grid.segments(
    y0 = grid::unit(at, "native"), 
    y1 = grid::unit(at, "native"), 
    x0 = tck.x0, 
    x1 = tck.x1
  )
  grid::grid.text(
    label = labels, 
    y = grid::unit(at, "native"), 
    x = text.x, 
    rot = rot, 
    just = text.just, 
    gp = grid::gpar(fontsize = label.size)
  )
}

#' @name grid.colorkey
#' @author mixfruit
#' @title Custom color key for grid plots
#' @description Create a custom color key for grid plots
#' @param x Data values for color scaling
#' @param color Color palette
#' @param color.n Number of color levels
#' @param ticks.side Side for ticks
#' @param pos Orientation (horizontal or vertical)
#' @return None
#' @export
grid.colorkey <- function(
  x = NULL, 
  color = NULL, 
  color.n = 100, 
  ticks.side = c("left", "right", "top", "bottom"), 
  pos = c("h", "v")
) {
  pos <- match.arg(pos, c("h", "v"))
  ticks.side <- match.arg(ticks.side, c("left", "right", "top", "bottom"))
  
  if (pos == "v") {
    x_scale <- c(0, 1)
    y_scale <- range(as.numeric(x))
    xpos <- 0.5
    ypos <- seq(0, 1, length = color.n)
    r_width <- grid::unit(1, "npc")
    r_height <- 1/(color.n - 1)
  } else {
    y_scale <- c(0, 1)
    x_scale <- range(as.numeric(x))
    ypos <- 0.5
    xpos <- seq(0, 1, length = color.n)
    r_width <- 1/(color.n - 1)
    r_height <- grid::unit(1, "npc")
  }
  
  grid::pushViewport(grid::viewport(angle = 0, yscale = y_scale, xscale = x_scale))
  
  if (is.null(color)) {
    cols <- c("blue", "white", "red")
  } else {
    cols <- color
  }
  
  col_p <- grDevices::colorRampPalette(cols)(color.n)
  
  if (pos == "v") {
    just <- c("bottom", rep("centre", color.n - 2), "top")
    for (i in 1:color.n) {
      grid::grid.rect(
        x = xpos, 
        y = ypos[i], 
        height = r_height, 
        width = r_width, 
        just = just[i], 
        gp = grid::gpar(col = col_p[i], fill = col_p[i])
      )
    }
  } else {
    just <- c("left", rep("centre", color.n - 2), "right")
    for (i in 1:color.n) {
      grid::grid.rect(
        x = xpos[i], 
        y = ypos, 
        height = r_height, 
        width = r_width, 
        just = just[i], 
        gp = grid::gpar(col = col_p[i], fill = col_p[i])
      )
    }
  }
  
  grid::grid.rect(gp = grid::gpar(fill = NA))
  
  if (pos == "h") {
    grid.xaxis2(side = ticks.side, tick.len = 0.25)
  } else {
    grid.yaxis2(side = ticks.side, tick.len = 0.25)
  }
  
  grid::popViewport()
}
