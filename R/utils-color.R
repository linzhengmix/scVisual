#' @name useMyCol
#' @author mixfruit
#' @title Custom color palette function
#' @description Generate a custom color palette
#' @param palette Palette name
#' @param n Number of colors to generate
#' @return A vector of colors
#' @export
useMyCol <- function(palette = "paired", n) {
  # Define color palettes
  palettes <- list(
    paired = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", 
               "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", 
               "#FFFF99", "#B15928"),
    stallion = c("#2B83BA", "#ABD9E9", "#FDAE61", "#F46D43", "#D7191C", 
                 "#A2142F", "#7B3294", "#C2A5CF", "#1B7837", "#7FC97F",
                 "#90C987", "#BECCAE"),
    default = grDevices::rainbow(n)
  )
  
  # Get the requested palette
  if (palette %in% names(palettes)) {
    pal <- palettes[[palette]]
  } else {
    pal <- palettes$default
  }
  
  # If we need more colors than the palette has, extend it
  if (n > length(pal)) {
    pal <- rep(pal, length.out = n)
  } else {
    pal <- pal[1:n]
  }
  
  return(pal)
}
