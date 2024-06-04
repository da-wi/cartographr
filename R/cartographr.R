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
  osm_object = osm

  options(warn=-1)

  suppressWarnings(suppressMessages(sf::sf_use_s2(FALSE)))

  # water
  osm_object$x.water$osm_lines1 <- osm_object$x.water$osm_lines
  osm_object$x.water$osm_polygons1 <- osm_object$x.water$osm_polygons
  osm_object$x.water$osm_multipolygons1 <- osm_object$x.water$osm_multipolygons
  osm_object$x.sea$osm_polygons1 <- osm_object$x.sea$osm_polygons
  osm_object$x.sea$osm_multipolygons1 <- osm_object$x.sea$osm_multipolygons

  tmp.water <- list(if (!is.null(osm_object$x.water$osm_lines1)) osm_object$x.water$osm_lines1 |> sf::st_make_valid(),
                    if (!is.null(osm_object$x.water$osm_polygons1)) osm_object$x.water$osm_polygons1  |> sf::st_make_valid(),
                    if (!is.null(osm_object$x.water$osm_multipolygons1)) osm_object$x.water$osm_multipolygons1 |> sf::st_make_valid(),
                    if (!is.null(osm_object$x.sea$osm_multipolygons1)) osm_object$x.sea$osm_multipolygons1 |> sf::st_make_valid(),
                    if (!is.null(osm_object$x.sea$osm_polygons1)) osm_object$x.sea$osm_polygons1 |> sf::st_make_valid())

  osm_object$water <- do.call(rbind, lapply(tmp.water, function(df) df[, Reduce(intersect, lapply(tmp.water, colnames))]))
  suppressMessages(osm_object$water.dis  <- sf::st_union(osm_object$water[1:dim(osm_object$water)[1],]))

  # buidlings
  osm_object$buildings <- list( osm_object$x.building$osm_polygons, osm_object$x.building$osm_multipolygons)
  osm_object$buildings <- osm_object$buildings[!sapply(osm_object$buildings,is.null)]
  osm_object$buildings.dis <- NULL
  if (length(osm_object$buildings) > 1) {
    #osm_object$buildings.dis <- dplyr::bind_rows(osm_object$buildings)
    osm_object$buildings.dis <- do.call(rbind, lapply(osm_object$buildings, function(df) df[, Reduce(intersect, lapply(osm_object$buildings, colnames))]))
  } else {
    osm_object$buildings.dis <- osm_object$buildings[[1]]
  }

  #if(!is.null(osm_object$x.building$osm_polygons)) osm_object$x.building$osm_polygons <-  osm_object$x.building$osm_polygons |>  sf::st_make_valid() |> sf::st_intersection(., crop_extent )
  #if(!is.null(osm_object$x.building$osm_multipolygons)) osm_object$x.building$osm_multipolygons <- osm_object$x.building$osm_multipolygons |>  sf::st_make_valid() |> sf::st_intersection(., crop_extent )

  osm_object$preprocessing <- "rect"
  osm_object$preprocessed <- TRUE
  return(osm_object)
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

  options(warn=-1)

  osm_object <- osm

  if (is.null(osm_object$preprocessed)) {
    osm_object <- preprocess_map(osm_object)
  }
  #osm = preprocess_map(osm) # should move to get_osmdata

  if (inherits(boundary, "sf") | inherits(boundary, "sfc")) {
    crop_extent = boundary
    osm_object$preprocessing <- "sf"
  }

  if (is.character(boundary)) {
    if (boundary == "circle") {
      crop_extent <- get_circle(osm_object$lat,osm_object$lon,osm_object$y_distance,osm_object$x_distance)
      osm_object$preprocessing <- "circle"
    }

    if (boundary == "hex") {
      crop_extent <- get_hexagon(osm_object$lat,osm_object$lon,osm_object$y_distance,osm_object$x_distance)
      osm_object$preprocessing <- "hex"
    }

    if (boundary == "rect") {
      crop_extent <- osm_object$bbox |> sf::st_as_sfc()
    }
  }

  # decrease margin for circle
  # scaling <- scaling/sqrt(2)

  osm_object$bbox <- sf::st_bbox(crop_extent)
  osm_object$bbox[c(1,2)] <- osm_object$bbox[c(1,2)]-(osm_object$bbox[c(3,4)]-osm_object$bbox[c(1,2)])*0.02
  osm_object$bbox[c(3,4)] <- osm_object$bbox[c(3,4)]+(osm_object$bbox[c(3,4)]-osm_object$bbox[c(1,2)])*0.02

  # streets
  if(!is.null(osm_object$x.street$osm_lines)) osm_object$x.street$osm_lines <- suppressMessages(osm_object$x.street$osm_lines |>  sf::st_make_valid() |> sf::st_intersection(x=_, crop_extent ))
  if(!is.null(osm_object$x.street$osm_points)) osm_object$x.street$osm_points <- suppressMessages(osm_object$x.street$osm_points |>  sf::st_make_valid() |> sf::st_intersection(x=_, crop_extent ))


  # buildings
  if(!is.null(osm_object$buildings.dis)) osm_object$buildings.dis <- suppressMessages(osm_object$buildings.dis |>  sf::st_make_valid() |> sf::st_intersection(x=_, crop_extent ))

  # TODO
  # for some magical reason, one needs to create a copy for water polygons to make it work. No idea why.
  #if(!is.null(osm_object$x.water$osm_lines)) osm_object$x.water$osm_lines1 <- osm_object$x.water$osm_lines |>  sf::st_make_valid() |> sf::st_intersection(., crop_extent )  |>  sf::st_make_valid()
  #if(!is.null(osm_object$x.water$osm_multipolygons)) osm_object$x.water$osm_multipolygons1 <- osm_object$x.water$osm_multipolygons |> sf::st_intersection(., crop_extent )
  #if(!is.null(osm_object$x.water$osm_polygons)) osm_object$x.water$osm_polygon1 <-  osm_object$x.water$osm_polygons |> sf::st_intersection(., crop_extent )
  #if(!is.null(osm_object$x.sea$osm_multipolygons)) osm_object$x.sea$osm_multipolygons1 <-  osm_object$x.sea$osm_multipolygons |>  sf::st_make_valid() |> sf::st_intersection(., crop_extent )
  #if(!is.null(osm_object$x.sea$osm_polygons)) osm_object$x.sea$osm_polygons1 <-  osm_object$x.sea$osm_polygons |>  sf::st_make_valid() |> sf::st_intersection(., crop_extent )

  if(!is.null(osm_object$water.dis)) osm_object$water.dis <- suppressMessages(osm_object$water.dis |>  sf::st_make_valid() |> sf::st_intersection(x=_, crop_extent )  |>  sf::st_make_valid())
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

  list(...)$osm
  plot <- .plot_map(...)

  plot + adjust_viewport(osm) + add_attribution()
}

#' Plot a map
#'
#' This function plots a map
#'
#' @param osm OSM object to plot
#' @param palette Color theme applied to the plot
#' @return NULL
#' @keywords internal
.plot_map = function(osm, palette = "imhof") {

  if (is.null(osm$preprocessed)) {
    osm <- preprocess_map(osm)
  }

  color = get_palette(palette)

  #scale_factor <- mean(scaling[1], scaling[2])
  scale_factor <- cartographr_env$scale_factor

  osm_object <- osm

  if (color$hatched) {
    # patterns for hatching
    df.point <- merge(data.frame( x = seq(osm_object$bbox$xmin,osm_object$bbox$xmax,(osm_object$bbox$xmax-osm_object$bbox$xmin)/300)),
          data.frame( y = seq(osm_object$bbox$ymin,osm_object$bbox$ymax,(osm_object$bbox$ymax-osm_object$bbox$ymin)/300)),all=TRUE)
    df.point <- df.point |> sf::st_as_sf(coords = c(1,2))
    sf::st_crs(df.point) <- 4326

    # df.point[lengths(sf::st_intersects(df.point,sf::st_crop(osm_object$water.dis,osm_object$bbox))) > 0,])

    # this is an expensive operation computationally
    #suppressMessages(df.point.gg <- sf::st_intersection(df.point,sf::st_crop(osm_object$water.dis,osm_object$bbox)))
    suppressMessages(df.point.gg <- sf::st_intersection(df.point,sf::st_crop(osm_object$water.dis,osm_object$bbox)))

  }
    # create a list of ggobjects for water
  gg.water <- list(ggplot2::geom_sf(data =osm_object$x.water$osm_lines1, fill = color$water, color= color$water, size = 1),
                   ggplot2::geom_sf(data =osm_object$x.water$osm_multipolygons1, fill = color$water, color =color$water),
                   ggplot2::geom_sf(data =osm_object$x.water$osm_polygons1, fill = color$water, color =color$water),
                   ggplot2::geom_sf(data =osm_object$x.sea$osm_polygons1, fill = color$water, color =color$water),
                   ggplot2::geom_sf(data =osm_object$x.sea$osm_multipolygons1, fill = color$water, color =color$water))

  # building border color
  if (!is.null(color$building_border))
    borderc = color$building_border
  else
    borderc = NA

  p <- ggplot2::ggplot() +
    # add background
    {if(osm_object$preprocessing %in% c("circle","hex","sf")) ggplot2::geom_sf(data=osm_object$crop_extent, fill=color$background,color=NA) else ggplot2::geom_sf(data=sf::st_as_sfc(osm_object$bbox), fill=color$background,color=NA)} +


    ### add layers on top
    # water
    # ATTENTION: when using {if} the original osm_object is used (important, when cropped!)
    # a list containing NULL will simply ignore NULL entries
    ggplot2::geom_sf(data =osm_object$water.dis, fill = color$water, color= color$water) +

    # water hatched
    {if(!is.null(osm_object$water.dis) && color$hatched == TRUE)  ggplot2::geom_sf( data=df.point.gg, shape=18,fill="black", size = 0.3, alpha=0.1 )}+

    # green, beach & parking
    ggplot2::geom_sf(data =osm_object$x.green$osm_multipolygons, fill = color$green, color=NA , linewidth=0.05) + # "#2F3737"
    ggplot2::geom_sf(data =osm_object$x.green$osm_polygons, fill = color$green, color= NA, linewidth=0.05) +
    ggplot2::geom_sf(data =osm_object$x.beach$osm_multipolygons, fill = color$beach, color= NA, linewidth=0.05) +
    ggplot2::geom_sf(data =osm_object$x.parking$osm_multipolygons, fill = color$parking, color= NA, linewidth=0.05) +

    # railway
    {if(!is.null(osm_object$x.railway$osm_lines))
      ggplot2::geom_sf(data =osm_object$x.railway$osm_lines |> subset(if (!is.null(osm_object$x.railway$osm_lines$tunnel)) is.na(osm_object$x.railway$osm_lines$tunnel) else rep(TRUE,dim(osm_object$x.railway$osm_lines)[1])),
                       linetype = "11",
                       color=color$railway, linewidth = 0.5)} +

    # streets
    ggplot2::geom_sf(data = subset(osm_object$x.street$osm_lines, osm$x.street$osm_lines$highway == "motorway") , color=color$street, linewidth=6*scale_factor) +
    ggplot2::geom_sf(data = subset(osm_object$x.street$osm_lines, osm$x.street$osm_lines$highway == "primary") , color=color$street, linewidth=4*scale_factor) +
    ggplot2::geom_sf(data = subset(osm_object$x.street$osm_lines, osm$x.street$osm_lines$highway == "secondary") , color=color$street, linewidth=4*scale_factor) +
    ggplot2::geom_sf(data = subset(osm_object$x.street$osm_lines, osm$x.street$osm_lines$highway == "tertiary") , color=color$street, linewidth=3*scale_factor) +
    ggplot2::geom_sf(data = subset(osm_object$x.street$osm_lines, osm$x.street$osm_lines$highway == "unclassified") , color=color$street, linewidth=3*scale_factor) +
    ggplot2::geom_sf(data = subset(osm_object$x.street$osm_lines, osm$x.street$osm_lines$highway == "residential") , color=color$street, linewidth=3*scale_factor) +
    ggplot2::geom_sf(data = subset(osm_object$x.street$osm_lines, osm$x.street$osm_lines$highway == "pedestrian") , color=color$street, linewidth=1*scale_factor) +
    ggplot2::geom_sf(data = subset(osm_object$x.street$osm_lines, osm$x.street$osm_lines$highway == "service") , color=color$street, linewidth=1*scale_factor) +
    ggplot2::geom_sf(data = subset(osm_object$x.street$osm_lines, osm$x.street$osm_lines$highway == "living_street"), color=color$street, linewidth=1*scale_factor) +
    {if(!is.null(color$lights)) ggplot2::geom_sf(data = osm_object$x.street$osm_points, color=color$lights, size=0.2*scale_factor)} +

    # buildings
    ggplot2::geom_sf(data = osm_object$buildings.dis, ggplot2::aes(fill = osm_object$buildings.dis$colors), show.legend = F, color= borderc, linewidth =0.01*scale_factor)+
    ggplot2::scale_fill_manual(values=color$palette_building)+

    # remove axes
    ggplot2::theme_void()+

    # add new scales
    ggnewscale::new_scale_color()+
    ggnewscale::new_scale_fill()

    options(warn=0)
    #p$scaling <- scaling

    p$scale_factor <- scale_factor
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
#' data("soho_boundary")
#' osm_data <- get_osmdata(lat = 48.2082, lon = 16.3738, x_distance = 1000)
#' osm_data <- get_osmdata(lat = 48.2082, lon = 16.3738, y_distance = 1000, aspect_ratio = 1.41)
#' osm_data <- get_osmdata(sf = soho_boundary)
#' }
#' @export
get_osmdata <- function(lat = NULL, lon = NULL, x_distance = NULL, y_distance = NULL, aspect_ratio = NULL, bbox = NULL, sf = NULL, quiet = F) {

  if (is.null(lat) && is.null(lon) && is.null(x_distance) && is.null(y_distance) && is.null(aspect_ratio) && is.null(bbox) && is.null(sf)) {
    stop(cli::cli_abort("At least one argument must be set"))
  }

  if (is.null(bbox) && is.null(sf)) {
    # Check the number of non-NULL arguments provided for y_distance, x_distance, and aspect_ratio
    num_args_provided <- sum(!is.null(c(y_distance, x_distance, aspect_ratio)))

    # If all three are provided, stop the function
    if (num_args_provided > 2) {
      stop(cli::cli_abort("Only two of 'y_distance', 'x_distance', and 'aspect_ratio' should be provided."))
    }

    if (num_args_provided == 0) {
      stop(cli::cli_abort("Please provide at least one of the arguments 'y_distance' or 'x_distance'."))
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
          stop(cli::cli_abort("set_output_size() must return a vector of length 2 to calculate aspect_ratio."))
      }
    }

    place <- get_border(as.numeric(lat),as.numeric(lon),y_distance,x_distance)
    coords_bbox <- as.numeric(strsplit(osmdata::opq(bbox = place)$bbox, split = ",")[[1]])
    coords_bbox[3]-coords_bbox[1]
    coords_bbox[4]-coords_bbox[2]


    bbox <- sf::st_bbox(c(xmin=coords_bbox[2],xmax=coords_bbox[4],ymin=coords_bbox[1],ymax=coords_bbox[3]), crs=sf::st_crs(4326))
  }

  # bbox explicitly given
  else {
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

  q.street <- osmdata::opq(bbox = place) |>
    osmdata::add_osm_feature("highway", c("motorway", "primary", "secondary", "tertiary", "unclassified", "residential","living_street","street_lamp", "pedestrian"))

  q.building <- osmdata::opq(bbox = place) |>
    osmdata::add_osm_feature("building")

  q.water <- osmdata::opq(bbox = place) |>
    osmdata::add_osm_feature("water")

  q.sea <- osmdata::opq(bbox = place) |>
    osmdata::add_osm_features(c(
      "\"place\"=\"sea\"",
      "\"place\"=\"ocean\"",
      "\"natural\"=\"water\"",
      "\"natural\"=\"strait\"",
      "\"natural\"=\"bay\""))

  q.green <- osmdata::opq(bbox = place) |>
    osmdata::add_osm_features (features = c (
      "\"landuse\"=\"forest\"",
      "\"landuse\"=\"grass\"",
      "\"landuse\"=\"orchard\"",
      "\"landuse\"=\"recreation_ground\"",
      "\"leisure\"=\"park\"",
      "\"natural\"=\"island\"",
      "\"natural\"=\"wood\""
    ))

  q.beach <- osmdata::opq(bbox = place) |>
    osmdata::add_osm_feature("natural", c("beach"))

  q.parking <- osmdata::opq (bbox=place) |>
    osmdata::add_osm_features (features = c (
      "\"amenity\"=\"parking\"",
      "\"highway\"=\"pedestrian\"",
      "\"man_made\"=\"pier\""
    ))

  q.railway <- osmdata::opq (bbox=place) |>
    osmdata::add_osm_features (features = c (
      "\"railway\"=\"rail\""
    ))


  if(!quiet) cli::cli_alert_info("Retrieving data, be patient with requests failing.")

  if (!is.null(bbox)) {
    if(!quiet) cli::cli_alert_info(paste0("xmin:",round(bbox[1],2),", ymin:",round(bbox[2],2),", xmax:",round(bbox[3],2),", ymax:", round(bbox[4],2)))
  }
  else {
    if(!quiet) cli::cli_alert_info(paste0("lat:",round(lat,2),", lon:",round(lon,2),", dy:",round(y_distance,2),", dx:", round(x_distance,2)))
  }
  osm <- c()

  if(!quiet) cli::cli_progress_step("Creating street network", spinner = T)

  osm$x.street <- q.street |> osmdata::osmdata_sf()

  osm$x.street$osm_lines_meters <- sf::st_transform(osm$x.street$osm_lines, crs = 32632)
  osm$x.street$osm_lines$length <- as.numeric(sf::st_length(osm$x.street$osm_lines_meters))
  length_quantile <- stats::quantile(osm$x.street$osm_lines$length, 0.25)
  osm$x.street$osm_lines <- subset(osm$x.street$osm_lines, length >= length_quantile)


  if(!quiet) cli::cli_progress_step("Constructing buildings", spinner = T)
  osm$x.building <- q.building |> osmdata::osmdata_sf()
  #osm$x.building$osm_polygons <- osm$x.building$osm_polygons |>
  ##  (\(x) if(!is.null(osm$x.building$osm_polygons$tunnel)) dplyr::filter( osm$x.building$osm_polygons, is.na(tunnel) == TRUE) else x)()
  #  (\(x) if(!is.null(osm$x.building$osm_polygons$tunnel)) subset(is.na(tunnel)) else x)()

  osm$x.building$osm_polygons <- osm$x.building$osm_polygons |>
    (\(x) if(!is.null(x$tunnel)) x[is.na(x$tunnel), ] else x)()

  if(!is.null(osm$x.building$osm_polygons)) osm$x.building$osm_polygons$colors <- sample(as.factor(c(1,2,3)) ,dim(osm$x.building$osm_polygons)[1], replace = T)
  if(!is.null(osm$x.building$osm_multipolygons)) osm$x.building$osm_multipolygons$colors <- sample(as.factor(c(1,2,3)) ,dim(osm$x.building$osm_multipolygons)[1], replace = T)

  if(!quiet) cli::cli_progress_step("Filling in water", spinner = T)
  osm$x.water <- q.water |> osmdata::osmdata_sf()
  osm$x.sea <- q.sea |> osmdata::osmdata_sf()

  if(!quiet) cli::cli_progress_step("Planting trees", spinner = T)
  osm$x.green <- q.green |> osmdata::osmdata_sf()


  if(!quiet) cli::cli_progress_step("Creating parking", spinner = T)
  osm$x.beach <- q.beach |> osmdata::osmdata_sf()
  osm$x.parking <- q.parking |> osmdata::osmdata_sf()
  osm$x.railway <- q.railway |> osmdata::osmdata_sf()

  osm$bbox <- bbox
  osm$y_distance <- y_distance
  osm$x_distance <- x_distance
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
    stop(cli::cli_abort("Orientation not recognized. Try 'portrait' or 'landscape'"))

  # if scale factors do not match anymore, we have to redraw the plot
  if (plot$scale_factor != cartographr_env$scale_factor) {
    cli::cli_alert_warning("`output_size` was changed after creating the plot, you might get unexpected results.")
  }

  ggplot2::ggsave(plot= plot,
                  filename = filename,
                  device = device,
                  width = cartographr_env$output_size[1],
                  height = cartographr_env$output_size[2],
                  units = "mm")
}
