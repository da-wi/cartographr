#' Preprocess OpenStreetMap data
#'
#' This function performs preprocessing on OpenStreetMap (OSM) data. It validates and
#' unifies various OSM elements such as water lines, polygons, and multipolygons, as well
#' as building polygons and multipolygons.
#'
#' @param osm A list object containing OSM data elements to be preprocessed.
#'
#' @return The preprocessed OSM object with validated and unified water bodies and buildings,
#'         and with additional preprocessing information.
#'
#' @examples
#' data("osm")
#' preprocessed_osm <- osm |> preprocess_map()
#' @export
preprocess_map = function(osm) {
  # copy osm_object
  # osm_object = osm

  options(warn=-1)

  # Streets
  osm$x.street <- list()
  if(!is.null(osm$x$osm_lines)) osm$x.street$osm_lines <- subset(osm$x$osm_lines, osm$x$osm_lines$highway %in%  c("motorway", "primary", "secondary", "tertiary", "unclassified", "residential","living_street","street_lamp", "pedestrian"))
  if(!is.null(osm$x$osm_points)) osm$x.street$osm_points <-osm$x$osm_points

  # Railway
  osm$x.railway <- list()
  if(!is.null(osm$x$osm_lines)) osm$x.railway$osm_lines <- subset(osm$x$osm_lines, osm$x$osm_lines$railway %in%  c("rail"))

  # Buildings
  osm$x.building <- list()
  if(!is.null(osm$x$osm_polygons)) osm$x.building$osm_polygons <- subset(osm$x$osm_polygons, osm$x$osm_polygons$building != "" )
  if(!is.null(osm$x$osm_multipolygons)) osm$x.building$osm_multipolygons <- subset(osm$x$osm_multipolygons, osm$x$osm_multipolygons$building != "")

  if(!is.null(osm$x.building$osm_polygons))  osm$x.building$osm_polygons <- osm$x.building$osm_polygons |>
    (\(x) if(!is.null(x$tunnel)) x[is.na(x$tunnel), ] else x)()

  #if(!is.null(osm$x.building$osm_polygons)) osm$x.building$osm_polygons$colors <- sample(as.factor(c(1,2,3)) ,dim(osm$x.building$osm_polygons)[1], replace = T)
  #if(!is.null(osm$x.building$osm_multipolygons)) osm$x.building$osm_multipolygons$colors <- sample(as.factor(c(1,2,3)) ,dim(osm$x.building$osm_multipolygons)[1], replace = T)

  # Water
  subset_water <- function(x) {
    tmp <- c()
    if(!(is.null(x$water))) { a <- subset(x, x$water != "" ); if(nrow(a) > 0) tmp <- tmp |> rbind(a) }
    return(tmp)
  }

  subset_sea <- function(x) {
    tmp <- c()
    if(!(is.null(x$place))) { a <- subset(x,x$place %in% c("sea","ocean")); if (nrow(a) > 0) tmp <- tmp |> rbind(a) }
    if(!(is.null(x$natural))) { a <- subset(x,x$natural %in% c("water","strait","bay")); if (nrow(a) > 0) tmp <- tmp |> rbind(a) }
    if(!(is.null(x$boundary))) { a <- subset(x,x$boundary %in% c("maritime")); if (nrow(a) > 0) tmp <- tmp |> rbind(a) }
    if(!(is.null(x$waterway))) { a <- subset(x,x$waterway %in% c("stream")); if (nrow(a) > 0) tmp <- tmp |> rbind(a) }
    return(tmp)
  }

  osm$x.water <- list()
  osm$x.sea <- list()
  if(!is.null(osm$x$osm_lines)) osm$x.water$osm_lines <- subset_water(osm$x$osm_lines)
  if(!is.null(osm$x$osm_polygons)) {
    osm$x.water$osm_polygons <- subset_water(osm$x$osm_polygons)
    osm$x.sea$osm_polygons <- subset_sea(osm$x$osm_polygons)
  }
  if(!is.null(osm$x$osm_multipolygons)) {
    osm$x.water$osm_multipolygons <- subset_water(osm$x$osm_multipolygons)
    osm$x.sea$osm_multipolygons <- subset_sea(osm$x$osm_multipolygons)
  }

  # Green
  subset_green <- function(x) {
    tmp <- c()
    if(!(is.null(x$landuse))) { a <- subset(x,x$landuse %in% c("forest","grass","orchard","recreation_ground")); if (nrow(a) > 0) tmp <- tmp |> rbind(a) }
    if(!(is.null(x$leisure))) { a <- subset(x,x$leisure %in% c("park")); if (nrow(a) > 0) tmp <- tmp |> rbind(a) }
    if(!(is.null(x$natural))) { a <- subset(x,x$natural %in% c("island", "wood")); if (nrow(a) > 0) tmp <- tmp |> rbind(a) }
    return(tmp)
  }

  osm$x.green <- list()
  if(!is.null(osm$x$osm_polygons)) osm$x.green$osm_polygons <- subset_green(osm$x$osm_polygons)
  if(!is.null(osm$x$osm_multipolygons)) osm$x.green$osm_multipolygons <- subset_green(osm$x$osm_multipolygons)

  # Beach
  osm$x.beach <- list()
  if(!is.null(osm$x$osm_multipolygons)) osm$x.beach$osm_multipolygons <- subset(osm$x.beach$osm_multipolygons,osm$x.beach$osm_multipolygons$natural %in% c("beach"))

  # Parking
  osm$x.parking <- list()
  subset_parking <- function(x) {
    tmp <- c()
    if(!(is.null(x$amenity))) { a <- subset(x,x$amenity %in% c("parking")); if (nrow(a) > 0) tmp <- tmp |> rbind(a) }
    if(!(is.null(x$highway))) { a <- subset(x,x$highway %in% c("pedestrian")); if (nrow(a) > 0) tmp <- tmp |> rbind(a) }
    if(!(is.null(x$man_made))) { a <- subset(x,x$man_made %in% c("pier")); if (nrow(a) > 0) tmp <- tmp |> rbind(a) }
    return(tmp)
  }

  if(!is.null(osm$x$osm_multipolygons)) osm$x.parking$osm_multipolygons <- subset_parking(osm$x$osm_multipolygons)

  # water
  water <- list(if (!is.null(osm$x.water$osm_lines)) osm$x.water$osm_lines |> sf::st_make_valid(),
                    if (!is.null(osm$x.water$osm_polygons)) osm$x.water$osm_polygons  |> sf::st_make_valid(),
                    if (!is.null(osm$x.water$osm_multipolygons)) osm$x.water$osm_multipolygons |> sf::st_make_valid(),
                    if (!is.null(osm$x.sea$osm_multipolygons)) osm$x.sea$osm_multipolygons |> sf::st_make_valid(),
                    if (!is.null(osm$x.sea$osm_polygons)) osm$x.sea$osm_polygons |> sf::st_make_valid())

  water <- do.call(rbind, lapply(water, function(df) df[, Reduce(intersect, lapply(water, colnames))]))

  if(!is.null(water)) { suppressMessages(osm$water.dis  <- sf::st_union(water[1:dim(water)[1],])) }

  # buildings
  buildings <- list( osm$x.building$osm_polygons, osm$x.building$osm_multipolygons)
  buildings <- buildings[!sapply(buildings,is.null)]
  osm$buildings.dis <- NULL
  if (length(buildings) > 1) {
    osm$buildings.dis <- do.call(rbind, lapply(buildings, function(df) df[, Reduce(intersect, lapply(buildings, colnames))]))
  } else {
    if(!is.null(buildings))
      osm$buildings.dis <- buildings[[1]]
  }

  # green
  green <- list( osm$x.green$osm_polygons, osm$x.green$osm_multipolygons)
  green <- green[!sapply(green,is.null)]
  osm$green.dis <- NULL
  if (length(green) > 1) {
    osm$green.dis <- do.call(rbind, lapply(green, function(df) df[, Reduce(intersect, lapply(green, colnames))]))
  } else {
    if(!is.null(green))
      osm$green.dis <- green[[1]]
  }

  osm$crop <- NA
  osm$preprocessed <- TRUE
  return(osm)
}

#' Create hatch patterns within a boundary
#'
#' This function generates hatch patterns within a given spatial boundary. The patterns can be generated as either points or lines, depending on the specified type.
#' @param boundary A spatial object representing the boundary within which to create the hatch pattern.
#' @param type A character string specifying the type of hatch pattern to create: "points" or "lines".
#' @param n_points The number of points to generate within the boundary when 'type' is "points". Default is 200.
#' @param n_lines The number of lines to generate within the boundary when 'type' is "lines". Default is 100.
#' @return A spatial object containing the generated hatch pattern.
#' @noRd
#' @keywords internal
.create_hatch_pattern <- function(boundary, type = "points", n_points = 200, n_lines = 100)  {

  # union to multipolygon if listtools:
  if (!is.null(nrow(boundary))) {
    suppressWarnings(suppressMessages({
      boundary <- sf::st_make_valid(boundary)
      boundary <- sf::st_union(boundary[1:dim(boundary)[1],])
    }))
  }

  if (type =="points") {
    suppressWarnings(suppressMessages({
      fillgrid <- rbind(sf::st_make_grid(boundary,
                                         n = n_points, what = c("centers")) |> sf::st_as_sf(),
                        sf::st_make_grid(boundary,
                                         n = n_points, what = c("corners")) |> sf::st_as_sf())


      fillgrid <- fillgrid[sf::st_contains(boundary, fillgrid, sparse = FALSE), ]
    }))

    return(fillgrid)
  }

  else if (type == "lines") {
    suppressWarnings(suppressMessages({
      fillgrid <- sf::st_make_grid(boundary,
                                   what = "polygons",
                                   square = T, n = n_lines
      )

      direction = list( horizontal = c(1, 2),
                        vertical = c(1, 4),
                        left2right = c(2, 4),
                        right2left = c(1, 3)
      )

      endsf = lapply(1:length(fillgrid), function(j)
        sf::st_linestring(
          sf::st_coordinates(
            fillgrid[j])[direction[[3]], 1:2]
        )
      )
      endsf = sf::st_sfc(endsf, crs = sf::st_crs(boundary))

      endsf = sf::st_intersection(endsf,boundary)
      endsf = endsf[sf::st_geometry_type(endsf)
                    %in% c("LINESTRING", "MULTILINESTRING")
      ]
      endsf = sf::st_line_merge(sf::st_union(endsf))
    }))

    return(endsf)
  }
}


#' Crop a preprocessed OpenStreetMap
#'
#' This function crops an OpenStreetMap (OSM) object that has been preprocessed.
#' It supports different types of geometric boundaries such as rectangles, circles, and hexagons,
#' or a custom boundary provided as an 'sf' object.
#'
#' @param osm A preprocessed OSM object to which the crop will be applied.
#' @param boundary The type of geometric boundary to apply to the OSM data.
#'   Can be "rect" for a rectangular boundary, "circle" for a circular boundary,
#'   "hex" for a hexagonal boundary, or an 'sf' object for a custom boundary.
#'   Default is "rect".
#'
#' @return The OSM object with the specified geometric crop applied.
#'
#' @details
#' If the OSM object has not been preprocessed, the function will call `preprocess_map()`
#' to preprocess the data before applying the cropping. The function sets global warning options
#' to suppress warnings during processing. The type of preprocessing applied is stored in the
#' OSM object's metadata.
#'
#' @examples
#' \donttest{
#' data("osm")
#' # Apply a circular crop
#' osm_circle_cropped <- osm |> crop(boundary = "circle")
#'
#' # Apply a hexagonal crop
#' osm_hex_cropped <- osm |> crop(boundary = "hex")
#'
#' # Apply a custom 'sf' boundary crop
#' data("soho_boundary")
#' osm_sf_cropped <- osm |> crop(boundary = soho_boundary)
#' }
#' @export
crop = function(osm, boundary = "rect") {

  if (is.null(osm)) {
    stop(cli::cli_abort("'osm' is not a valid object."))
  }

  if (is.null(boundary)) {
    stop(cli::cli_abort("boundary is not a valid character or 'sf' object."))
  }

  options(warn=-1)

  osm_object <- osm

  if (is.null(osm_object$preprocessed)) {
    osm_object <- preprocess_map(osm_object)
  }

  if (inherits(boundary, "sf") | inherits(boundary, "sfc")) {
    crop_extent = boundary
    osm_object$crop <- "sf"
  }

  if (is.character(boundary)) {
    if (boundary == "circle") {
      crop_extent <- get_circle(osm_object$lat,osm_object$lon,osm_object$y_distance,osm_object$x_distance)
      osm_object$crop <- "circle"
    }

    if (boundary == "hex") {
      crop_extent <- get_hexagon(osm_object$lat,osm_object$lon,osm_object$y_distance,osm_object$x_distance)
      osm_object$crop <- "hex"
    }

    if (boundary == "rect") {
      crop_extent <- osm_object$bbox |> sf::st_as_sfc()
      osm_object$crop <- "rect"
    }
  }

  osm_object$bbox <- sf::st_bbox(crop_extent)

  # streets
  if(!is.null(osm_object$x.street$osm_lines)) osm_object$x.street$osm_lines <- suppressMessages(osm_object$x.street$osm_lines |>  sf::st_make_valid() |> sf::st_intersection(x=_, crop_extent ))
  if(!is.null(osm_object$x.street$osm_points)) osm_object$x.street$osm_points <- suppressMessages(osm_object$x.street$osm_points |>  sf::st_make_valid() |> sf::st_intersection(x=_, crop_extent ))

  # buildings
  if(!is.null(osm_object$buildings.dis)) osm_object$buildings.dis <- suppressMessages(osm_object$buildings.dis |>  sf::st_make_valid() |> sf::st_intersection(x=_, crop_extent ))

  # water
  if(!is.null(osm_object$water.dis)) osm_object$water.dis <- suppressMessages(osm_object$water.dis |>  sf::st_make_valid() |> sf::st_intersection(x=_, crop_extent )  |>  sf::st_make_valid())

  # green
  if(!is.null(osm_object$x.green$osm_multipolygons)) suppressMessages(osm_object$x.green$osm_multipolygons <-  osm_object$x.green$osm_multipolygons |>  sf::st_make_valid() |> sf::st_intersection(x=_, crop_extent ))
  if(!is.null(osm_object$x.green$osm_polygons)) suppressMessages(osm_object$x.green$osm_polygons <-  osm_object$x.green$osm_polygons |>  sf::st_make_valid() |> sf::st_intersection(x=_, crop_extent ))

  if(!is.null(osm_object$x.beach$osm_multipolygons)) suppressMessages(osm_object$x.beach$osm_multipolygons <-  osm_object$x.beach$osm_multipolygons |>  sf::st_make_valid() |> sf::st_intersection(x=_, crop_extent ))
  if(!is.null(osm_object$x.parking$osm_multipolygons)) suppressMessages(osm_object$x.parking$osm_multipolygons <-  osm_object$x.parking$osm_multipolygons |>  sf::st_make_valid() |> sf::st_intersection(x=_, crop_extent ))

  if(!is.null(osm_object$x.railway$osm_lines))  suppressMessages(osm_object$x.railway$osm_lines <- osm_object$x.railway$osm_lines |>  sf::st_make_valid() |> sf::st_intersection(x=_, crop_extent ))

  if (!is.null(crop_extent)) osm_object$crop_extent <- crop_extent

  options(warn=1)
  return(osm_object)
}


#' Plot a map with custom palette
#'
#' This function takes an 'osmdata' (osm) object and a palette name, preprocesses the map data if not already done, and plots the map using 'ggplot2' with the specified color palette.
#'
#' @param ... Variable argument list:
#'   - `osm`: A list retrieved from osmdata containing map data.
#'   - `palette`: A character string specifying the name of the palette to use. The default is "imhof".
#'   Additional arguments are passed on to the preprocessing and plotting functions.
#'
#' @return A `ggplot` object representing the map with the chosen palette.
#' @examples
#' data("osm")
#' my_map <- osm |> plot_map()
#' my_map <- osm |> plot_map(palette = 'gray')
#'
#' @export
plot_map <- function(...) {
  # Call the original plot_map function
  args <- list(...)
  if ("osm" %in% names(args)) {
    osm <- args[["osm"]]
  } else if (is.list(args[[1]])) {
    osm <- args[[1]]
  } else {
    stop(cli::cli_abort("Input should be a list or contain a named argument 'osm'."))
  }

  plot <- .plot_map(...)

  plot + adjust_viewport(plot) + add_attribution()

}



#' Plot a map
#'
#' This function plots a map
#'
#' @param osm OSM object to plot
#' @param palette Color theme applied to the plot
#' @return NULL
#' @keywords internal
#' @noRd
.plot_map = function(osm, palette = "imhof") {

  if (is.null(osm$preprocessed)) {
    osm <- preprocess_map(osm)
  }

  color = get_palette(palette)

  scale_factor <- cartographr_env$scale_factor

  osm_object <- osm

  if (color$hatch_water) {
      # water can extend a lot outside of bbox.. so we crop it for convencience
      pattern_water <- .create_hatch_pattern(boundary = suppressMessages(sf::st_crop(osm_object$water.dis |> sf::st_make_valid(),osm_object$bbox)),
                                            type = color$hatch_water_type,
                                            n_points = color$hatch_water_npoints,
                                            n_lines  = color$hatch_water_nlines)
  }

  if (color$hatch_buildings) {
    pattern_buildings <- .create_hatch_pattern( suppressMessages(sf::st_crop(osm_object$buildings.dis |> sf::st_make_valid(),osm_object$bbox)),
                                              type = color$hatch_buildings_type,
                                              n_points = color$hatch_buildings_npoints,
                                              n_lines  = color$hatch_buildings_nlines)
  }

  if (color$hatch_green) {
    pattern_green <- .create_hatch_pattern(suppressMessages(sf::st_crop(osm_object$green.dis |> sf::st_make_valid(),osm_object$bbox)),
                                          type = color$hatch_green_type,
                                          n_points = color$hatch_green_npoints,
                                          n_lines  = color$hatch_green_nlines)
  }

  ########
  # create a border around the map?
  if (is.null(osm_object$crop_extent))
    osm_object$crop_extent <- osm_object$bbox |> sf::st_as_sfc()

  frame <- NULL
  if (!is.null(color$border_color)) {
    suppressWarnings({
    width = ifelse(is.null(color$border_width),0.001,color$border_width)
    projected_shape <- sf::st_transform(osm_object$crop_extent, crs = "+proj=utm +zone=33 +datum=WGS84")

    buffered_projected_shape <- sf::st_buffer(projected_shape, dist = sqrt((sf::st_bbox(projected_shape)[3]-sf::st_bbox(projected_shape)[1])^2+
                                                                             (sf::st_bbox(projected_shape)[4]-sf::st_bbox(projected_shape)[2])^2)*width)
    buffered_shape <- sf::st_transform(buffered_projected_shape, crs = sf::st_crs(osm_object$crop_extent))
    frame <- suppressMessages(sf::st_difference(buffered_shape, osm_object$crop_extent))
    })
    # set p <- sf::st_bbox(frame) at the end to adjust_viewport() correctly
  }
  ###########

  # colors for buildings
  if(!is.null(osm_object$buildings.dis)) {
    osm_object$buildings.dis$colors <- sample(color$palette_building,dim(osm_object$buildings.dis)[1], replace = T)
  }


  p <- ggplot2::ggplot() +
    # add background
    {if(osm_object$crop %in% c("circle","hex","sf")) ggplot2::geom_sf(data=osm_object$crop_extent, fill=color$background,color=NA) else ggplot2::geom_sf(data=sf::st_as_sfc(osm_object$bbox), fill=color$background,color=NA)} +

    ### add layers on top
    # water
    # ATTENTION: when using {if} the original osm_object is used (important, when cropped!)
    # a list containing NULL will simply ignore NULL entries
    ggplot2::geom_sf(data =osm_object$water.dis, fill = color$water, color= color$water) +

    # water hatched
    {if(!is.null(osm_object$water.dis) && color$hatch_water == TRUE)  ggplot2::geom_sf( data=pattern_water, shape=18,fill="black", size = color$hatch_water_size*scale_factor, alpha=color$hatch_water_alpha )}+

    # green, beach & parking
    ggplot2::geom_sf(data =osm_object$x.beach$osm_multipolygons, fill = color$beach, color= NA, linewidth=0.05) +
    ggplot2::geom_sf(data =osm_object$x.parking$osm_multipolygons, fill = color$parking, color= NA, linewidth=0.05) +
    ggplot2::geom_sf(data =osm_object$x.green$osm_multipolygons, fill = color$green, color=NA , linewidth=0.05) +
    ggplot2::geom_sf(data =osm_object$x.green$osm_polygons, fill = color$green, color= NA, linewidth=0.05) +

    {if(!is.null(osm_object$green.dis) && color$hatch_green == TRUE)  ggplot2::geom_sf( data=pattern_green, shape=18,fill="black", size = color$hatch_green_size*scale_factor,linewidth = color$hatch_green_size*scale_factor, alpha=color$hatch_green_alpha )}+

    # railway
    {if(!is.null(osm_object$x.railway$osm_lines))
      ggplot2::geom_sf(data =osm_object$x.railway$osm_lines |> subset(if (!is.null(osm_object$x.railway$osm_lines$tunnel)) is.na(osm_object$x.railway$osm_lines$tunnel) else rep(TRUE,dim(osm_object$x.railway$osm_lines)[1])),
                       linetype = "solid",
                       color=color$railway, linewidth = 2*scale_factor)} +

    # streets
    ggplot2::geom_sf(data = subset(osm_object$x.street$osm_lines, osm$x.street$osm_lines$highway == "motorway") , color=color$street, linewidth=color$linewidth_motorway*scale_factor) +
    ggplot2::geom_sf(data = subset(osm_object$x.street$osm_lines, osm$x.street$osm_lines$highway == "primary") , color=color$street, linewidth=color$linewidth_primary*scale_factor) +
    ggplot2::geom_sf(data = subset(osm_object$x.street$osm_lines, osm$x.street$osm_lines$highway == "secondary") , color=color$street, linewidth=color$linewidth_secondary*scale_factor) +
    ggplot2::geom_sf(data = subset(osm_object$x.street$osm_lines, osm$x.street$osm_lines$highway == "tertiary") , color=color$street, linewidth=color$linewidth_tertiary*scale_factor) +
    ggplot2::geom_sf(data = subset(osm_object$x.street$osm_lines, osm$x.street$osm_lines$highway == "unclassified") , color=color$street, linewidth=color$linewidth_unclassified*scale_factor) +
    ggplot2::geom_sf(data = subset(osm_object$x.street$osm_lines, osm$x.street$osm_lines$highway == "residential") , color=color$street, linewidth=color$linewidth_residential*scale_factor) +
    ggplot2::geom_sf(data = subset(osm_object$x.street$osm_lines, osm$x.street$osm_lines$highway == "pedestrian") , color=color$street, linewidth=color$linewidth_pedestrian*scale_factor) +
    ggplot2::geom_sf(data = subset(osm_object$x.street$osm_lines, osm$x.street$osm_lines$highway == "service") , color=color$street, linewidth=color$linewidth_service*scale_factor) +
    ggplot2::geom_sf(data = subset(osm_object$x.street$osm_lines, osm$x.street$osm_lines$highway == "living_street"), color=color$street, linewidth=color$linewidth_living_street*scale_factor) +
    {if(!is.null(color$lights)) ggplot2::geom_sf(data = osm_object$x.street$osm_points, color=color$lights, size=color$size_streetlamp*scale_factor)} +

    # buildings
    ggplot2::geom_sf(data = osm_object$buildings.dis, fill = osm_object$buildings.dis$colors, show.legend = F, color = ifelse(is.null(color$building_border), NA, color$building_border), linewidth =0.05*scale_factor)+
    {if(!is.null(osm_object$buildings.dis) && color$hatch_buildings == TRUE)  ggplot2::geom_sf(data = pattern_buildings, shape=18,fill="black", size = color$hatch_buildings_size*scale_factor,linewidth = color$hatch_buildings_size*scale_factor, alpha=color$hatch_buildings_alpha )}+


    # border
    ggplot2::geom_sf(data=frame, fill = "black", color = NA) +

    ggplot2::scale_fill_manual(values=color$palette_building)+

    # remove axes
    ggplot2::theme_void()

    options(warn=0)

    # store scale_factor for check if output size has changed
    p$scale_factor <- scale_factor

    # store bbox for adjust_viewport()
    if(is.null(frame))
      p$bbox <- osm_object$bbox
    else
      p$bbox <- sf::st_bbox(frame)


    return(p)
}


#' Retrieve OpenStreetMap data for a specified location
#'
#' This function fetches OpenStreetMap (OSM) data for a specified latitude and longitude,
#' within a given distance range. It allows the user to specify distances in the x and y
#' directions, an aspect ratio, or a combination of these to define the area for which
#' OSM data is retrieved. The function ensures that only two of the three parameters
#' (x_distance, y_distance, aspect_ratio) are provided to define the area.
#'
#' @param lat Optional numeric, the latitude of the center point for the OSM data retrieval.
#' @param lon Optional numeric, the longitude of the center point for the OSM data retrieval.
#' @param x_distance Optional numeric, the distance in meters from the center point
#'   to the edge of the area in the x direction (longitude). Default is NULL.
#' @param y_distance Optional numeric, the distance in meters from the center point
#'   to the edge of the area in the y direction (latitude). Default is NULL.
#' @param aspect_ratio Optional numeric, the ratio of x_distance to y_distance.
#'   Default is NULL.
#' @param sf Optional sf of the map boundary.
#'   Default is NULL.
#' @param bbox Optional bbox of the map boundary.
#'   Default is NULL.
#' @param quiet Logical, whether to suppress informational messages during execution.
#'   Default is FALSE.
#'
#' @return A list containing various elements of the OSM data, including street networks,
#'   buildings, water bodies, green areas, beaches, parking areas, railways, and the
#'   bounding box of the retrieved area.
#'
#' @examples
#' \donttest{
#' osm_data <- get_osmdata(lat = 48.2082, lon = 16.3738, x_distance = 10)
#' }
#' @export
get_osmdata <- function(lat = NULL, lon = NULL, x_distance = NULL, y_distance = NULL, aspect_ratio = NULL, bbox = NULL, sf = NULL, quiet = F) {

  if (is.null(lat) && is.null(lon) && is.null(x_distance) && is.null(y_distance) && is.null(aspect_ratio) && is.null(bbox) && is.null(sf)) {
    stop(cli::cli_abort("At least one argument must be set"))
  }

  calculated_bbox <- T

  if (is.null(bbox) && is.null(sf)) {
    # Check the number of non-NULL arguments provided for y_distance, x_distance, and aspect_ratio
    num_args_provided <- sum(!is.null(c(y_distance, x_distance, aspect_ratio)))

    # If all three are provided, stop the function
    if (num_args_provided > 2) {
      stop(cli::cli_abort("Only two of 'y_distance', 'x_distance', and 'aspect_ratio' should be provided."))
    }

    # If only two are provided, calculate the third variable
    if (!is.null(y_distance) && !is.null(x_distance)) {
      aspect_ratio <- x_distance / y_distance
    } else if (!is.null(y_distance) && !is.null(aspect_ratio)) {
      x_distance <- y_distance * aspect_ratio
    } else if (!is.null(x_distance) && !is.null(aspect_ratio)) {
      y_distance <- x_distance / aspect_ratio
    }

    # If only one of x_distance and y_distance is provided without aspect_ratio,
    # try to calculate the aspect_ratio from the vector of length 2 returned by set_output_size()
    if (is.null(aspect_ratio) && (is.null(y_distance) || is.null(x_distance))) {
      output_size <- set_output_size() # Assuming get_output_size() returns a vector of length 2
      if (length(output_size) == 2) {
        aspect_ratio <- output_size[1] / output_size[2]
        if (is.null(x_distance)) {
          x_distance <- y_distance * aspect_ratio
        } else {
          y_distance <- x_distance / aspect_ratio
        }
      } else {
        stop(cli::cli_abort("`set_output_size()` must return a vector of length 2 to calculate aspect_ratio."))
      }
    }

    place <- get_border(as.numeric(lat),as.numeric(lon),y_distance,x_distance)
    coords_bbox <- as.numeric(strsplit(osmdata::opq(bbox = place)$bbox, split = ",")[[1]])

    bbox <- sf::st_bbox(c(xmin=coords_bbox[2],xmax=coords_bbox[4],ymin=coords_bbox[1],ymax=coords_bbox[3]), crs=sf::st_crs(4326))
  }

  # bbox explicitly given
  else {
    calculated_bbox <- F
    if (is.null(sf)) {
      place <- bbox
    } else {
      if (is.null(bbox)) {
        place <- sf::st_bbox(sf)
        bbox <- place
      }
    }

    lon <- (bbox$xmin + bbox$xmax) / 2
    lat <- (bbox$ymin + bbox$ymax) / 2

    suppressWarnings(suppressMessages(sf::sf_use_s2(TRUE)))
    x_distance <- sf::st_distance(sf::st_as_sf(data.frame(x = bbox[1], y = bbox[2]+(bbox[4]-bbox[2])/2), coords = c("x", "y"), crs = 4326), sf::st_as_sf(data.frame(x = bbox[3], y = bbox[2]+(bbox[4]-bbox[2])/2), coords = c("x", "y"), crs = 4326))
    y_distance <- sf::st_distance(sf::st_as_sf(data.frame(x = bbox[2], y = bbox[1]+(bbox[3]-bbox[1])/2), coords = c("y", "x"), crs = 4326), sf::st_as_sf(data.frame(x = bbox[4], y = bbox[1]+(bbox[3]-bbox[1])/2), coords = c("y", "x"), crs = 4326))
    suppressWarnings(suppressMessages(sf::sf_use_s2(FALSE)))

    #centroid_of_bbox <- c((bbox$xmin + bbox$xmax) / 2, (bbox$ymin + bbox$ymax) / 2)
    #y_distance <- haversine_distance(centroid_of_bbox[2], centroid_of_bbox[1], bbox$ymax, centroid_of_bbox[1]) +
    #  haversine_distance(centroid_of_bbox[2], centroid_of_bbox[1], bbox$ymin, centroid_of_bbox[1])
  }

  query <- osmdata::opq(bbox = place) |>
    osmdata::add_osm_features(list("highway" =
                                     c("motorway", "primary", "secondary",
                                       "tertiary", "unclassified", "residential",
                                       "living_street","street_lamp", "pedestrian"),
                                   "water" = c(),
                                   "building" = c(),
                                   "natural" = "beach",
                                   "amenity" = "parking",
                                   "man_made" = "pier",
                                   "railway" = "rail",
                                   "place" = "sea",
                                   "place" = "ocean",
                                   "natural"= "water",
                                   "natural" = "strait",
                                   "natural" = "bay",
                                   "boundary" = "maritime",
                                   "waterway" = "stream",
                                   "landuse"="forest",
                                   "landuse"="grass",
                                   "landuse"="orchard",
                                   "landuse"="recreation_ground",
                                   "leisure"="park",
                                   "natural"="island",
                                   "natural"="wood"))

  osm <- c()

  if(!quiet) cli::cli_alert_info("Retrieving data, be patient, on public servers it can last up to one minute")

  if (calculated_bbox) {
    if(!quiet) cli::cli_alert_info(paste0("xmin:",round(bbox[1],2),", ymin:",round(bbox[2],2),", xmax:",round(bbox[3],2),", ymax:", round(bbox[4],2)))
  }
  else {
    if(!quiet) cli::cli_alert_info(paste0("lat:",round(lat,2),", lon:",round(lon,2),", dy:",round(y_distance,2),", dx:", round(x_distance,2)))
  }

  # Retrieve data (can last 30s)
  osm$x <- query |> osmdata::osmdata_sf(quiet = T)



  osm$bbox <- bbox
  osm$y_distance <- y_distance
  osm$x_distance <- x_distance
  osm$aspect_ratio <- aspect_ratio
  osm$lat <- lat
  osm$lon <- lon
  if(!quiet) cli::cli_progress_done()

  if(!quiet) cli::cli_alert_success(crayon::blue("Complete."))


  return(osm)
}


#' Save a map to a file
#'
#' This function saves a ggplot object to a file using the specified filename. It checks for the orientation setting and warns if the scale factor has changed after the plot was created.
#'
#' @param plot A ggplot object representing the map to be saved.
#' @param filename A character string specifying the path and name of the file to save the plot to.
#' @param device The output device defaulting to pdf
#' @return The function saves the plot to a file and does not return anything.
#'
#' @examples
#' \donttest{
#' data("osm")
#' my_map <- osm |> plot_map()
#' filename <- tempfile(fileext =  ".pdf")
#' save_map(my_map, filename)
#' unlink(filename)
#' }
#' @export
save_map <- function(plot, filename, device = "pdf") {
  if(!cartographr_env$orientation %in% c('portrait','landscape'))
    stop(cli::cli_abort('Orientation not recognized. Try "portrait" or "landscape"'))

  # if scale factors do not match anymore, we have to redraw the plot
  if (plot$scale_factor != cartographr_env$scale_factor) {
    cli::cli_alert_warning("'output_size' was changed after creating the plot, you might get unexpected results.")
  }

  ggplot2::ggsave(plot= plot,
                  filename = filename,
                  device = device,
                  width = cartographr_env$output_size[1],
                  height = cartographr_env$output_size[2],
                  units = "mm")
}
