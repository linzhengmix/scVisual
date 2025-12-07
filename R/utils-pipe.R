#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' @title Get ordered levels from a vector
#' @description Get levels from factor if available, otherwise unique values.
#' This function maintains the custom order of factors and provides consistent
#' behavior across different data types.
#' @param x A vector to process
#' @return Ordered levels maintaining factor order if factor, otherwise unique values
#' @keywords internal
get_ordered_levels <- function(x) {
  if (is.factor(x)) {
    levels(x)
  } else {
    unique(x)
  }
}

#' @title Get first level maintaining factor order
#' @description Get first level preserving factor order if factor,
#' otherwise first unique value. This ensures consistent behavior for
#' corner axes placement and other ordered operations.
#' @param x A vector to process
#' @return First level as factor if original was factor, otherwise unique value
#' @keywords internal
get_first_level <- function(x) {
  if (is.factor(x)) {
    factor(levels(x)[1], levels = levels(x))
  } else {
    unique(x)[1]
  }
}

#' @title Validate and warn about factor status
#' @description Check if a column is a factor and provide helpful warning
#' if not, suggesting factor conversion for custom ordering.
#' @param data Data frame containing the column
#' @param colname Name of the column to check
#' @param context Context for the warning message
#' @keywords internal
validate_factor_support <- function(data, colname, context = "plotting") {
  if (!is.null(colname) && colname %in% colnames(data)) {
    if (!is.factor(data[[colname]])) {
      warning(sprintf(
        "Column '%s' is not a factor. Consider converting to factor for custom ordering in %s. Example: data$%s <- factor(data$%s, levels = c('desired_order'))",
        colname, context, colname, colname
      ))
    }
  }
}
