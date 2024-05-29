#' Package Configuration Environment
#'
#' This environment stores configuration settings for the package.
#' It is initialized when the package is loaded and can be modified
#' using the `set_*()` functions.
#'
#' @name cartographr_env
#' @keywords internal
cartographr_env <- new.env(parent = emptyenv())
cartographr_env$output_size <- c(210,297)
cartographr_env$scale_factor <- 0.2497607
cartographr_env$orientation <- "portrait"

#' Set Output Size for cartography
#'
#' This function sets the output size for cartographic displays. It allows the user to specify a standard paper size or custom dimensions. If no size is specified, it returns the current output size.
#'
#' @param size A character string specifying the standard paper size or a numeric vector with custom dimensions (width, height). The standard sizes can be one of "A0", "A1", "A2", "A3", "A4", "A5", "A6", "small_poster", "medium_poster", or "large_poster". If size is NULL, the current output size is returned.
#'
#' @return If size is NULL, returns the current output size as a numeric vector. If a size is specified, the function sets the output size..
#'
#' @examples
#' set_output_size("A3") # Sets the output size to A3 dimensions
#' set_output_size(c(300, 200)) # Sets a custom output size
#'
#' @export
set_output_size <- function(size = NULL, orientation = "portrait") {
  if(!orientation %in% c('portrait','landscape'))
    stop(cli::cli_abort("Orientation not recognized. Try 'portrait' or 'landscape'"))

  if(is.null(size))
    return(cartographr_env$output_size)

  dims <- list(
    A0 = c(841, 1189),
    A1 = c(594, 841),
    A2 = c(420, 594),
    A3 = c(297, 420),
    A4 = c(210, 297),
    A5 = c(148, 210),
    A6 = c(105, 148),
    small_poster = c(432, 279),   # 11 x 17in converted to mm
    medium_poster = c(609.6, 457.2), # 18 x 24in converted to mm
    large_poster = c(914.4, 609.6)  # 24 x 36in converted to mm
  )

  if (is.character(size)) {
    if (!size %in% names(dims)) {
      stop(cli::cli_abort("Format not recognized. Try: A0, A1, A2, A3, A4, A5, A6, small_poster, medium_poster, large_poster"))
    }
    else {
      cartographr_env$output_size <- dims[[size]]
    }
  }

  if (is.numeric(size)) {
    if (length(size) != 2) {
      stop(cli::cli_abort("Lenght of vector `format` must be exactly 2."))
    }
    else {
      cartographr_env$output_size <- size
    }
  }

  if (grid::is.unit(size)) {
    if (length(size) != 2) {
      stop(cli::cli_abort("Lenght of vector `format` must be exactly 2."))
    }
    else {
      cartographr_env$output_size <- as.numeric(grid::convertUnit(size, "mm"))
    }
  }

  if(orientation == 'landscape') {
    cartographr_env$output_size = rev(cartographr_env$output_size)
  }

  ref_dims <- c(841, 1189)
  ref_scale <- sqrt(ref_dims[1]^2 + ref_dims[2]^2)

  cartographr_env$scale_factor <- sqrt(cartographr_env$output_size[1]^2 +
                                         cartographr_env$output_size[2]^2) / ref_scale

  cartographr_env$orientation <- orientation
  invisible()
}


