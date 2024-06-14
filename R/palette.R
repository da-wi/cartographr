#' Create a palette for maps
#'
#' This function creates a color theme to be used with `plot_map()`. It can accept a predefined palette name or a custom palette provided as a named list.
#'
#' @param palette The color palette to use.
#' Can be one of "alphabet", "arctic", "autumn", "autumn-muted", "bw", "desert", "evening", "gray", "iberia", "imhof", "lines", "metropolitan", "midnight", "minimal", "swiss", "tropical", or a named list for a custom palette.
#' If a named list is provided, it should contain color hex codes for each map element.
#' If `NULL` or an unrecognized name is provided, the function will throw an error.
#' @return A list containing color settings for the map elements.
#'
#' @details
#' The color moods for the predefined palettes are described as follows:
#' - **Alphabet**: A modern palette with a straightforward aesthetic.
#' - **Arctic**: A palette that reflects the clear and bright qualities of Arctic landscapes.
#' - **Autumn**: A palette with the warm and varied hues typical of the fall season.
#' - **BW**: A classic black and white palette with a hint of warmth for a traditional feel.
#' - **Evening**: A palette that embodies the quiet and contemplative nature of dusk.
#' - **Gray**: A balanced palette that provides a composed and refined look.
#' - **Iberia**: A palette that reflects the warm and diverse tones associated with the Iberian landscape.
#' - **Imhof**: A palette with natural and subdued tones, inspired by the work of cartographer Eduard Imhof.
#' - **Lines (BW)**: A contrasting black and white palette for a clear and defined appearance.
#' - **Metropolitan**: A palette with understated tones that suggest urban sophistication.
#' - **Midnight**: A palette that conveys the depth and tranquility of the night.
#' - **Minimal**: A palette focused on minimalism, utilizing primarily whites and light grays.
#' - **Serene**: A palette that embodies peace and simplicity, utilizing a soft color scheme with gentle contrasts.
#' - **Swiss**: A palette that emphasizes cleanliness and precision, reminiscent of Swiss design.
#' - **Tropical**: A lively palette with the bright and bold colors characteristic of tropical areas.
#'
#' In addition, you can customize other settings:
#' - `border_color`: The color of the borders, set to a dark shade "#121212".
#' - `border_width`: The width of the borders, set to a very fine line of 0.001 units.
#' - `linewidth_buildings`: The line width for building outlines, set to 0.05 units.
#' - `linewidth_motorway`, `linewidth_trunk`, `linewidth_primary`, `linewidth_secondary`, `linewidth_tertiary`, `linewidth_unclassified`, `linewidth_residential`: The line widths for various types of roads, ranging from 6 units for motorways to 1 unit for pedestrian paths.
#' - `size_streetlamp`: The size representation for streetlamps, set to 0.2 units.
#' - `hatch_*`: A series of settings for hatching patterns, which can be applied to water, buildings, and green spaces. These include toggles for hatching (`hatch_water`, `hatch_buildings`, `hatch_green`), the number of points or lines (`hatch_*_npoints`, `hatch_*_nlines`), the type of hatching pattern (`hatch_*_type`), the size of the hatching elements (`hatch_*_size`), and the transparency level (`hatch_*_alpha`).
#'
#' These settings allow for a high degree of customization when creating maps, providing users with the ability to fine-tune the appearance of their map elements according to their specific needs or preferences.
#'
#' @examples
#' # Use predefined palette
#' get_palette("imhof")
#'
#' # Custom palette creation using a named list for a simple black and white palette
#' custom_palette <- list(
#'   palette_building = c("#000000", "#FFFFFF", "#CCCCCC"),
#'   water = "#000000",
#'   green = "#FFFFFF",
#'   beach = "#000000",
#'   parking = "#FFFFFF",
#'   street = "#000000",
#'   background = "#CCCCCC",
#'   railway = "#000000",
#'   hatch_water = TRUE,
#'   linewidth_buildings = 0.05,
#'   linewidth_motorway = 6,
#'   linewidth_primary = 4,
#'   linewidth_secondary = 4,
#'   linewidth_tertiary=3,
#'   linewidth_unclassified = 3,
#'   linewidth_residential = 3,
#'   linewidth_pedestrian = 1,
#'   linewidth_service = 1,
#'   linewidth_living_street = 1,
#'   size_hatch = 1,
#'   alpha_hatch = 0.1,
#'   size_streetlamp = 0.2
#' )
#'
#' get_palette(custom_palette)
#' @export
get_palette = function(palette) {
  sizes <- list(name = palette,
                border_color = "#121212",
                border_width = 0.001,

                linewidth_buildings = 0.05,
                linewidth_motorway = 6,
                linewidth_trunk = 6,
                linewidth_primary = 4,
                linewidth_secondary = 4,
                linewidth_tertiary= 3,
                linewidth_unclassified = 3,
                linewidth_residential = 3,
                linewidth_pedestrian = 1,
                linewidth_service = 1,
                linewidth_living_street = 1,
                size_streetlamp = 0.2,

                hatch_water = FALSE,
                hatch_water_npoints = 200,
                hatch_water_nlines = 100,
                hatch_water_type = "points",
                hatch_water_size = 1,
                hatch_water_alpha = 0.1,
                hatch_buildings = FALSE,
                hatch_buildings_npoints = 200,
                hatch_buildings_nlines = 100,
                hatch_buildings_type = "points",
                hatch_buildings_size = 1,
                hatch_buildings_alpha = 0.1,
                hatch_green = FALSE,
                hatch_green_npoints = 200,
                hatch_green_nlines = 100,
                hatch_green_type = "lines",
                hatch_green_size = 1,
                hatch_green_alpha = 0.1
  )

  if (is.list(palette) && all(names(palette) != "")) {
    if(!all(c("water", "street", "palette_building", "green", "background","parking","railway", "beach") %in% names(palette)))
      stop(cli::cli_abort("Required fields for custom palette are 'background', 'beach', 'green', 'parking', 'palette_building', 'railway','street','water'"))

    palette$name <- "custom"
    color <- utils::modifyList(sizes, palette)
    class(color) <- "cartographr_palette"
    return(color)
  }

  if (!(palette %in% c( "alphabet", "arctic","autumn", "bw","dotted",
                        "desert","evening", "gray","grey", "iberia", "imhof","lines","midnight",
                        "mikimal","minimal","metropolitan","serene","swiss","tropical")))
    stop(cli::cli_abort(paste(palette,"is not a predefined palette.")))



  color <- list()

  if (palette == "alphabet") {
    color <- list(
      palette_building = c("#f2eeea", "#dad2c6", "#ded2c4"),
      water = "#5a95b8",
      street = "#fefefe",
      green = "#9bc2a6",
      background = "#eceeec",
      parking = "#fefefe",
      railway =  "#c2c2bf",
      beach = "#ded2c4"
    )
  }

  if (palette == "arctic") {
    color <- list(
      palette_building = c("#e0e4e8", "#e0e4e8", "#e0e4e8"),
      building_border = "#ccd6dd",
      water = "#a9c0d1",
      street = "#f0f5f7",
      green = "#d8e2e7",
      background = "#f0f5f7",
      parking = "#f0f5f7",
      railway = "#c2d1d9",
      beach = "#e0e4e8"
    )
  }

  if (palette == "autumn") {
    color <- list(
      palette_building = c("#ff643d", "#80321F", "#E65A37"),
      railway = "#a38263",
      green = "#d2d8b3",
      water = "#a1bcae",
      background = "#fff5d9",
      street = "#fff5d9",
      beach = "#fff5d9",
      parking = "#fff5d9",
      hatch_water = TRUE,
      hatch_green = TRUE
    )
  }

  if (palette == "bw") {
    color <- list(
      palette_building = c("#292e28", "#292e28", "#292e28"),
      railway = "#faf5eb",
      green = "#faf5eb",
      water = "#faf5eb",
      background = "#292e28",
      street = "#faf5eb",
      beach = "#faf5eb",
      parking = "#faf5eb"
    )
  }

  if (palette == "evening") {
    color <- list(
      palette_building = c("#855988", "#6B4984", "#483475"),
      water = "#192058",
      green = "#2B2F77",
      railway = "#1b1b1b",
      lights = "#F7E7C2",
      beach = "#2F2352",
      parking = "#2F2352",
      street = "#1C1F31",
      background = "#060A2E"
    )
  }

  if (palette %in% c("gray","grey")) {
    color <- list(
      palette_building = c("#d9d9d9", "#d9d9d9", "#d9d9d9"),
      building_border = "#bdbdbd",
      water = "#a6a6a6",
      street = "#f0f0f0",
      green = "#cccccc",
      background = "#f0f0f0",
      parking = "#f0f0f0",
      railway = "#999999",
      beach = "#d9d9d9"
    )
  }

  if (palette == "iberia") {
    color <- list(
      background = "#F2F4CB",
      water = "#a8e1e6",
      green = "#8BB174",
      streets = "#3F4747",
      palette_building = c("#433633", "#433633", "#FF5E5B"),
      beach = "#FCE19C",
      parking = "#F2F4CB",
      railway = "#1b1b1b"
    )
  }

  if (palette == "imhof") {
    color <- list(
      palette_building = c("#7e6e55", "#9c8c6e", "#6a5944"),
      railway = "#a35e48",
      green = "#c5d1a5",
      water = "#9dbcd4",
      background = "#f3efe2",
      street = "#b0a18f",
      beach = "#e2d1b3",
      parking = "#a9a18c"
    )
  }

  if (palette == "lines") {
    color <- list(
      palette_building = c("#FFFFFF", "#FFFFFF", "#FFFFFF"),
      railway = "#292e28",
      green = "#FFFFFF",
      water = "#FFFFFF",
      background = "#ffffff",
      street = "#292e28",
      beach = "#ffffff",
      parking = "#ffffff"
    )
  }

  if (palette == "metropolitan") {
    color <- list(
      palette_building = c("#e0d7be", "#e0d7be", "#e0d7be"),
      building_border = "#c9c0af",
      water = "#9db9a4",
      street = "#f4f2e1",
      green = "#d9dcb7",
      background = "#f4f2e1",
      parking = "#f4f2e1",
      railway = "#c4c4bc",
      beach = "#e6d5c3"
    )
  }

  if (palette == "midnight") {
    color <- list(
      palette_building = c("#333333", "#333333", "#333333"),
      building_border = NULL,
      water = "#2d2d2d",
      green = "#393939",
      railway = "#121212",
      lights = "#ffee90",
      beach = "#333333",
      parking = "#333333",
      street = "#1c1c1c",
      background = "#181818"
    )
  }

  if (palette == "minimal") {
    color <- list(
      palette_building = c("#ffffff", "#ffffff", "#ffffff"),
      building_border = "#000000",
      water = "#e6e6e6",
      street = "#ffffff",
      green = "#f2f2f2",
      background = "#ffffff",
      parking = "#ffffff",
      railway = "#d0d0d0",
      beach = "#ffffff"
    )
  }


  if (palette == "serene") {
    color <- list(
      palette_building = "#fefefd",
      building_border = "#d7d7d7",
      water = "#D5D5D9", #"#7a9aab",
      street = "#ffffff",
      green = "#BCB9B0",#"#c8ddc7",
      background = "#fbfbf9",
      parking = "#d3d3d5",
      railway = "#cdcdd3",
      beach = "#ffffff",
      hatch_buildings = TRUE,
      hatch_buildings_alpha = 0.66,
      hatch_buildings_size = 0.00001,
      hatch_buildings_npoints = 280,
      hatch_buildings_nlines = 100,
      hatch_buildings_type = "points",
      hatch_green = TRUE,
      hatch_green_type = "lines",
      hatch_green_alpha = 0.75,
      hatch_green_nlines = 125,
      hatch_green_size = 0.05
    )
  }

  if (palette == "swiss") {
    color <- list(
      palette_building = c("#cdc4b9", "#cdc4b9", "#d9d9d9"),
      building_border = "#bdbdbd",
      water = "#d1dfe6",
      street = "#fbfbf9",
      green = "#dbe5d0",
      background = "#fbfbf9",
      parking = "#fbfbf9",
      railway = "#999999",
      beach = "#fbfbf9"
    )
  }

  if (palette == "tropical") {
    color <- list(
      palette_building = c("#FFC857", "#E9724C", "#C5283D"),
      water = "#a8e1e6",
      green = "#8BB174",
      beach = "#FCE19C",
      parking = "#F2F4CB",
      street = "#475657",
      background = "#F2F4CB",
      railway = "#586F6F"
    )
  }

  color <-  utils::modifyList(sizes, color)

  class(color) <- "cartographr_palette"

  return(color)
}
