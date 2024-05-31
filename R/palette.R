#' Create a color theme for maps
#'
#' This function creates a color theme to be used with `plot_map()`. It can accept a predefined palette name or a custom palette provided as a named list.
#'
#' @param palette The color palette to use.
#' Can be one of "gray", "swiss", "evening", "midnight", "lines", "minimal", "bw", "arctic", "autumn", "autumn-muted", "desert", "metropolitan", "imhof", "alphabet", "tropical", "iberia", or a named list for a custom palette.
#' If a named list is provided, it should contain color hex codes for each map element.
#' If `NULL` or an unrecognized name is provided, the function will throw an error.
#' @return A list containing color settings for the map elements.
#'
#' @details
#' The color moods for the predefined palettes are as follows:
#' - **Gray**: A neutral and balanced palette that conveys a sense of calm and sophistication.
#' - **Swiss**: A clean and orderly palette reflecting the precision and clarity of Swiss design.
#' - **Evening**: A deep and moody palette that captures the tranquil and reflective moments of the evening.
#' - **Midnight**: A dark and mysterious palette, perfect for conveying the stillness and depth of the night.
#' - **Lines (BW)**: A stark and classic black and white palette, offering a timeless and elegant look.
#' - **Minimal**: A minimalist palette with a focus on simplicity and clarity, using mostly white and subtle grays.
#' - **BW**: A black and white palette with a warm undertone, providing a vintage and nostalgic feel.
#' - **Arctic**: A cool and refreshing palette that evokes the crisp and pristine landscapes of the Arctic.
#' - **Autumn**: A warm and inviting palette that captures the rich and vibrant colors of the fall season.
#' - **Autumn Muted**: A softer take on the autumn palette, with muted tones that create a cozy atmosphere.
#' - **Desert**: A warm and earthy palette inspired by the natural colors of the desert landscape.
#' - **Metropolitan**: A sophisticated urban palette with muted tones and refined elegance.
#' - **Imhof**: A palette inspired by the renowned cartographer Eduard Imhof, featuring earthy tones for a natural look.
#' - **Alphabet**: A playful and modern palette with a clean and approachable aesthetic.
#' - **Tropical**: A vibrant and lively palette that brings the energetic and colorful vibe of tropical regions.
#' - **Iberia**: A palette inspired by the warm and rich colors of the Iberian Peninsula, full of life and energy.
#'
#' @examples
#' \dontrun{
#' # Standard usage with a character argument for a predefined palette
#' plot_map(osm, palette = 'tropical')
#'
#' # Custom palette creation using a named list for a simple black and white palette
#' custom_palette <- list(
#'   palette_building = c("#000000", "#FFFFFF"),
#'   water = "#000000",
#'   green = "#FFFFFF",
#'   beach = "#000000",
#'   parking = "#FFFFFF",
#'   street = "#000000",
#'   background = "#FFFFFF",
#'   railway = "#000000",
#'   hatched = TRUE
#' )
#' plot_map(osm, palette = custom_palette)
#' }
#' @export
get_palette = function(palette) {
  if (is.list(palette) && all(names(palette) != "")) {
    color <- palette
    return(color)
  }

  if (!(palette %in% c( "alphabet",   "arctic","autumn", "autumn-muted","bw",
                        "desert","evening", "gray", "iberia", "imhof","lines","midnight",
                        "minimal","metropolitan","swiss","tropical")))
    stop(cli::cli_abort(paste(palette,"is not a predefined palette.")))

  color <- list()

  color$name <- palette

  if (palette == "gray") {
    color$palette_building <- c("#d9d9d9", "#d9d9d9", "#d9d9d9")
    color$building_border <- "#bdbdbd"
    color$water <- "#a6a6a6"
    color$street <- "#f0f0f0"
    color$green <- "#cccccc"
    color$background <- "#f0f0f0"
    color$parking <- "#f0f0f0"
    color$railway <- "#999999"
    color$hatched <- FALSE
    color$beach <- "#d9d9d9"
    color$parking <- "#f0f0f0"
  }

  if (palette == "swiss") {
    color$palette_building <- c("#cdc4b9", "#cdc4b9", "#d9d9d9")
    color$building_border <- "#bdbdbd"
    color$water <- "#d1dfe6"
    color$street <- "#fbfbf9"
    color$green <- "#dbe5d0"
    color$background <- "#fbfbf9"
    color$parking <- "#fbfbf9"
    color$railway <- "#999999"
    color$hatched <- FALSE
    color$beach <- "#fbfbf9"
    color$parking <- "#fbfbf9"
  }

  if (palette == "evening") {
    color$palette_building = c("#855988","#6B4984", "#483475")
    color$water <- "#192058"
    color$green <- "#2B2F77"
    color$railway <- "#1b1b1b"
    color$lights <- "#F7E7C2" # "#f7f7c8"
    color$beach <- "#2F2352"
    color$parking <- "#2F2352"
    color$street <- "#1C1F31"
    color$background <- "#060A2E"
    color$hatched <- FALSE
  }

  if (palette == "midnight") {
    color$palette_building <- c("#333333", "#333333", "#333333")
    color$building_border <- NULL
    color$water <- "#2d2d2d"
    color$green <- "#393939"
    color$railway <- "#121212"
    color$lights <- "#ffee90"
    color$beach <- "#333333"
    color$parking <- "#333333"
    color$street <- "#1c1c1c"
    color$background <- "#181818"
    color$hatched <- FALSE
  }

  # BW
  if (palette == "lines") {
    color$palette_building = c("#FFFFFF","#FFFFFF", "#FFFFFF")
    color$railway <- "#292e28"
    color$green <- "#FFFFFF"
    color$water <- "#FFFFFF" #"#97B2B9"
    color$background <- "#ffffff" # "#D3CCAF" #"#192058"
    color$street <- "#292e28"
    color$beach <- "#ffffff"
    color$parking <- "#Ffffff"
    color$hatched <- FALSE
  }

  if (palette == "minimal") {
    color$palette_building <- c("#ffffff", "#ffffff", "#ffffff")
    color$building_border <- "#000000"
    color$water <- "#e6e6e6"
    color$street <- "#ffffff"
    color$green <- "#f2f2f2"
    color$background <- "#ffffff"
    color$parking <- "#ffffff"
    color$railway <- "#4d4d4d"
    color$hatched <- FALSE
    color$beach <- "#ffffff"
    color$parking <- "#ffffff"
  }

  if (palette == "bw") {
    color$palette_building = c("#292e28","#292e28", "#292e28")
    color$railway <- "#faf5eb"#FFFFFF"
    color$green <- "#faf5eb"
    color$water <- "#faf5eb" #"#97B2B9"
    color$background <- "#292e28" # "#D3CCAF" #"#192058"
    color$street <- "#faf5eb"
    color$beach <- "#faf5eb"
    color$parking <- "#faf5eb"
    color$hatched <- FALSE
  }

  if (palette == "arctic") {
    color$palette_building <- c("#e0e4e8", "#e0e4e8", "#e0e4e8")
    color$building_border <- "#ccd6dd"
    color$water <- "#a9c0d1"
    color$street <- "#f0f5f7"
    color$green <- "#d8e2e7"
    color$background <- "#f0f5f7"
    color$parking <- "#f0f5f7"
    color$railway <- "#c2d1d9"
    color$hatched <- FALSE
    color$beach <- "#e0e4e8"
    color$parking <- "#f0f5f7"
  }


  if (palette == "autumn") {
    color$palette_building = c("#ff643d","#80321F", "#E65A37")
    color$railway <- "#a38263"
    color$green <- "#d2d8b3"
    color$water <- "#a1bcae" #"#97B2B9"
    color$background <- "#fff5d9"
    color$street <- "#fff5d9"
    color$beach <- "#fff5d9"
    color$parking <- "#fff5d9"
    color$hatched <- TRUE
  }

  if (palette == "autumn-muted") {
    color$palette_building <- c("#e6b8af", "#70534a", "#cc8066")
    color$railway <- "#9c8577"
    color$green <- "#c6ccb9"
    color$water <- "#b0c4be"
    color$background <- "#f7f3e9"
    color$street <- "#f7f3e9"
    color$beach <- "#f7f3e9"
    color$parking <- "#f7f3e9"
    color$hatched <- TRUE
  }

  if (palette == "desert") {
    color$palette_building <- c("#e8d9c3", "#e8d9c3", "#e8d9c3")
    color$building_border <- "#d3c2a6"
    color$water <- "#c2b19d"
    color$street <- "#f0e8d7"
    color$green <- "#ded5c5"
    color$background <- "#f0e8d7"
    color$parking <- "#f0e8d7"
    color$railway <- "#b2a690"
    color$hatched <- FALSE
    color$beach <- "#e8d9c3"
    color$parking <- "#f0e8d7"
  }

  if (palette == "metropolitan") {
    color$palette_building <- c("#ded9c6","#ded9c6","#ded9c6")
    color$building_border <- c("#c7c3b2")
    color$water <- c("#a3c1ad")
    color$street <- c("#f2f0d7")
    color$green <- c("#dbddb5")
    color$background <- c("#f2f0d7")
    color$parking <- c("#f2f0d7")
    color$railway <- c("#c2c2bf")
    color$hatched <- FALSE
    color$beach <- "#ded2c4"
    color$parking <- "#f2f0d7"
  }

  if (palette == "imhof") {
    color$palette_building = c("#73664d","#88754E", "#5A4925")
    color$railway <- "#bd4833"
    color$green <- "#bdddb0"
    color$water <- "#eefaee" #"#97B2B9"
    color$background <- "#fef7d1" # "#D3CCAF" #"#192058"
    color$street <- "#bdddb0" # "#B4D3DB"
    color$beach <- "#fef7d1"
    color$parking <- "#bdddb0"
    color$hatched <- FALSE
  }

  if (palette == "alphabet") {
    color$palette_building <- c("#f2eeea","#dad2c6","#ded2c4")
    color$water <- c("#5a95b8")
    color$street <- c("#fefefe")
    color$green <- c("#9bc2a6")
    color$background <- c("#eceeec")
    color$parking <- c("#f1f3f2")
    color$railway <- c("#c2c2bf")
    color$hatched <- FALSE
    color$beach <- "#ded2c4"
    color$parking <- "#fefefe"
  }

  if (palette == "tropical") {
    # MACAU
    color$palette_building = c("#FFC857","#E9724C","#C5283D")
    color$water <- "#a8e1e6"
    color$green <- "#8BB174"
    color$beach <- "#FCE19C"
    color$parking <- "#F2F4CB"
    color$street <- "#475657"
    color$background <- "#F2F4CB"
    color$railway <- "#475657"
    color$hatched <- TRUE
  }

  if (palette == "iberia") {
    color$background <- "#F2F4CB"
    color$water <- "#a8e1e6"
    color$green <- "#8BB174"
    color$streets <- "#2F3737"
    color$palette_building <- c("#433633","#433633", "#FF5E5B")
    color$beach <- "#FCE19C"
    color$parking <- "#F2F4CB"
    color$railway <- "#1b1b1b"
    color$hatched <- TRUE
  }

  return(color)
}
