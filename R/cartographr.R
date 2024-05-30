#' Preprocessing map
#'
#' This function uses sensible defaults to remove unnecessary data from the osm object
#' and sets the output size for printing.
#'
#' @param osm The osm_object with stored bbox
#' @return A preprocessed osm object
#' @export
preprocess_map = function(osm) {
  # copy osm_object
  osm_object = osm

  # scaling factor of output format for adjusting outer margins
  # scale_factor <- mean(scaling[1],scaling[2])
  options(warn=-1)

  sf::sf_use_s2(F)

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
  osm_object$buildings <- list( osm_object$x1$osm_polygons, osm_object$x1$osm_multipolygons)
  osm_object$buildings <- osm_object$buildings[!sapply(osm_object$buildings,is.null)]
  osm_object$buildings.dis <- NULL
  if (length(osm_object$buildings) > 1) {
    #osm_object$buildings.dis <- dplyr::bind_rows(osm_object$buildings)
    osm_object$buildings.dis <- do.call(rbind, lapply(osm_object$buildings, function(df) df[, Reduce(intersect, lapply(osm_object$buildings, colnames))]))
  } else {
    osm_object$buildings.dis <- osm_object$buildings[[1]]
  }

  #if(!is.null(osm_object$x1$osm_polygons)) osm_object$x1$osm_polygons <-  osm_object$x1$osm_polygons |>  sf::st_make_valid() |> sf::st_intersection(., cutout_extent )
  #if(!is.null(osm_object$x1$osm_multipolygons)) osm_object$x1$osm_multipolygons <- osm_object$x1$osm_multipolygons |>  sf::st_make_valid() |> sf::st_intersection(., cutout_extent )

  #osm_object$preprocessing <- "rectangular"
  osm_object$preprocessing <- "rect"
  osm_object$preprocessed <- TRUE
  # osm_object <- cutout(osm_object)
  return(osm_object)
}


#' Preprocessing of a circular map
#'
#' This function uses sensible defaults to remove unnecessary data from the osm object
#'
#' @param osm The osm_object with stored bbox
#' @return A preprocessed osm object
#' @export
cutout = function(osm, boundary = "rect") {

  options(warn=-1)

  osm_object <- osm

  if (is.null(osm_object$preprocessed)) {
    osm_object <- preprocess_map(osm_object)
  }
  #osm = preprocess_map(osm) # should move to get_osmdata

  if (class(boundary) == "sf") {
    cutout_extent = cutout
    osm_object$preprocessing <- "sf"
  }

  if (class(boundary) == "character") {
    if (boundary == "circle") {
      cutout_extent <- get_circle(osm_object$lat,osm_object$lon,osm_object$y_distance,osm_object$x_distance)
      osm_object$preprocessing <- "circle"
    }

    if (boundary == "hex") {
      cutout_extent <- get_circle(osm_object$lat,osm_object$lon,osm_object$y_distance,osm_object$x_distance)
      osm_object$preprocessing <- "hex"
    }

    if (boundary == "rect") {
      cutout_extent <- osm_object$bbox |> sf::st_as_sfc()
    }
  }

  # decrease margin for circle
  # scaling <- scaling/sqrt(2)

  osm_object$bbox <- sf::st_bbox(cutout_extent)
  osm_object$bbox[c(1,2)] <- osm_object$bbox[c(1,2)]-(osm_object$bbox[c(3,4)]-osm_object$bbox[c(1,2)])*0.02
  osm_object$bbox[c(3,4)] <- osm_object$bbox[c(3,4)]+(osm_object$bbox[c(3,4)]-osm_object$bbox[c(1,2)])*0.02

  # streets
  if(!is.null(osm_object$x$osm_lines)) osm_object$x$osm_lines <- suppressMessages(osm_object$x$osm_lines |>  sf::st_make_valid() |> sf::st_intersection(x=_, cutout_extent ))
  if(!is.null(osm_object$x$osm_points)) osm_object$x$osm_points <- suppressMessages(osm_object$x$osm_points |>  sf::st_make_valid() |> sf::st_intersection(x=_, cutout_extent ))


  # buildings
  if(!is.null(osm_object$buildings.dis)) osm_object$buildings.dis <- suppressMessages(osm_object$buildings.dis |>  sf::st_make_valid() |> sf::st_intersection(x=_, cutout_extent ))

  # TODO
  # for some magical reason, one needs to create a copy for water polygons to make it work. No idea why.
  #if(!is.null(osm_object$x.water$osm_lines)) osm_object$x.water$osm_lines1 <- osm_object$x.water$osm_lines |>  sf::st_make_valid() |> sf::st_intersection(., cutout_extent )  |>  sf::st_make_valid()
  #if(!is.null(osm_object$x.water$osm_multipolygons)) osm_object$x.water$osm_multipolygons1 <- osm_object$x.water$osm_multipolygons |> sf::st_intersection(., cutout_extent )
  #if(!is.null(osm_object$x.water$osm_polygons)) osm_object$x.water$osm_polygon1 <-  osm_object$x.water$osm_polygons |> sf::st_intersection(., cutout_extent )
  #if(!is.null(osm_object$x.sea$osm_multipolygons)) osm_object$x.sea$osm_multipolygons1 <-  osm_object$x.sea$osm_multipolygons |>  sf::st_make_valid() |> sf::st_intersection(., cutout_extent )
  #if(!is.null(osm_object$x.sea$osm_polygons)) osm_object$x.sea$osm_polygons1 <-  osm_object$x.sea$osm_polygons |>  sf::st_make_valid() |> sf::st_intersection(., cutout_extent )

  if(!is.null(osm_object$water.dis)) osm_object$water.dis <- suppressMessages(osm_object$water.dis |>  sf::st_make_valid() |> sf::st_intersection(x=_, cutout_extent )  |>  sf::st_make_valid())
  if(!is.null(osm_object$x.green$osm_multipolygons)) suppressMessages(osm_object$x.green$osm_multipolygons <-  osm_object$x.green$osm_multipolygons |>  sf::st_make_valid() |> sf::st_intersection(x=_, cutout_extent ))
  if(!is.null(osm_object$x.green$osm_polygons)) suppressMessages(osm_object$x.green$osm_polygons <-  osm_object$x.green$osm_polygons |>  sf::st_make_valid() |> sf::st_intersection(x=_, cutout_extent ))

  if(!is.null(osm_object$x.beach$osm_multipolygons)) suppressMessages(osm_object$x.beach$osm_multipolygons <-  osm_object$x.beach$osm_multipolygons |>  sf::st_make_valid() |> sf::st_intersection(x=_, cutout_extent ))
  if(!is.null(osm_object$x.parking$osm_multipolygons)) suppressMessages(osm_object$x.parking$osm_multipolygons <-  osm_object$x.parking$osm_multipolygons |>  sf::st_make_valid() |> sf::st_intersection(x=_, cutout_extent ))

  if(!is.null(osm_object$x.railway$osm_lines))  suppressMessages(osm_object$x.railway$osm_lines <- osm_object$x.railway$osm_lines |>  sf::st_make_valid() |> sf::st_intersection(x=_, cutout_extent ))

  if (!is.null(cutout_extent)) osm_object$cutout_extent <- cutout_extent

  options(warn=1)
  return(osm_object)
}



#' Print Custom ggplot Object
#'
#' This function takes a custom ggplot object, extracts the ggplot object,
#' retrieves the bounding box (bbox) from the associated osm data, and adjusts
#' the display before printing the plot.
#'
#' @param x A custom ggplot object with an attribute "osm" containing osm data.
#' @param ... Additional arguments passed to the print method.
#'
#' @return The function does not return a value but prints the ggplot object.
#' @export
print.cartographr_ggplot <- function(x, ...) {
  # print(x)
  # Extract the ggplot object from the custom class
  plot <- x

  # Get the bounding box (bbox) from the osm data
  osm <- attr(x,"osm")

  # Add coord_sf with the calculated limits
  plot <- plot + adjust_display(osm)  + add_acknowledgments()

  plot(plot)
}

#' Wrapper function for plot_map
#'
#' This function plots a map
#'
#' @param osm OSM object to plot
#' @param palette Color theme applied to the plot
#' @return NULL
#' @export
plot_map <- function(osm, palette = "imhof") {
  # Call the original plot_map function
  plot <- .plot_map(osm, palette) +
    ggnewscale::new_scale_color()+
    ggnewscale::new_scale_fill()

  # Return a custom class object containing the plot and osm data
  # for implicity adjusting the display
  structure(plot, class = c("cartographr_ggplot", class(plot)), osm = osm)
}

#' Plot a map
#'
#' This function plots a map
#'
#' @param osm OSM object to plot
#' @param palette Color theme applied to the plot
#' @return NULL
#' @export
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

  p <- ggplot2::ggplot() +
    # add background
    {if(osm_object$preprocessing == "circle") ggplot2::geom_sf(data=osm_object$cutout_extent, fill=color$background,color=NA) else ggplot2::geom_sf(data=sf::st_as_sfc(osm_object$bbox), fill=color$background,color=NA)} +


    ### add layers on top
    # water
    # BEWARE: when using {if} the original osm_object is used (important, when cropped!)
    # a list containing NULL will simply ignore NULL entries
    ggplot2::geom_sf(data =osm_object$water.dis, fill = color$water, color= color$water) +

    # water hatched
    {if(!is.null(osm_object$water.dis) && color$hatched == TRUE)  ggplot2::geom_sf( data=df.point.gg, shape=18,fill="black", size = 0.3, alpha=0.1 )}+
    #{if(!is.null(osm_object$x.water$osm_multipolygons1) && color$hatched == TRUE) ggplot2::geom_sf( data=df.point[sf::st_intersects(df.point,osm_object$x.water$osm_multipolygons1) |> lengths > 0,], shape=18,fill="black", size = 0.3, alpha=0.1 )} +
    #{if(!is.null(osm_object$x.water$osm_polygons1) && color$hatched == TRUE) ggplot2::geom_sf( data=df.point[sf::st_intersects(df.point,osm_object$x.water$osm_polygons1) |> lengths > 0,], shape=18, fill="black",size = 0.3, alpha=0.1 )} +
    #{if(!is.null(osm_object$x.sea$osm_multipolygons1) && color$hatched == TRUE) ggplot2::geom_sf( data=df.point[sf::st_intersects(df.point,osm_object$x.sea$osm_multipolygons1) |> lengths > 0,], shape=18,fill="black", size = 0.3, alpha=0.1 )} +
    #{if(!is.null(osm_object$x.sea$osm_polygons1) && color$hatched == TRUE) ggplot2::geom_sf( data=df.point[sf::st_intersects(df.point,osm_object$x.sea$osm_polygons1) |> lengths > 0,], shape=18, fill="black",size = 0.3, alpha=0.1 )} +

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
    ggplot2::geom_sf(data =osm_object$x$osm_lines |> subset(highway == "motorway") , color=color$street, linewidth=6*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines |> subset(highway == "primary") , color=color$street, linewidth=4*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines |> subset(highway == "secondary") , color=color$street, linewidth=4*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines |> subset(highway == "tertiary") , color=color$street, linewidth=3*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines |> subset(highway == "unclassified") , color=color$street, linewidth=3*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines |> subset(highway == "residential") , color=color$street, linewidth=3*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines |> subset(highway == "pedestrian") , color=color$street, linewidth=1*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines |> subset(highway == "service") , color=color$street, linewidth=1*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines |> subset(highway == "living_street") , color=color$street, linewidth=1*scale_factor) +
    {if(!is.null(color$lights)) ggplot2::geom_sf(data = osm_object$x$osm_points, color=color$lights, size=0.2*scale_factor)} +

    # buildings
    #ggplot2::geom_sf(data =osm_object$x1$osm_multipolygons, ggplot2::aes(fill = colors),show.legend = F, color= NA, linewidth = 0*scale_factor)+
    #ggplot2::geom_sf(data =osm_object$x1$osm_polygons, ggplot2::aes(fill = colors), show.legend = F,color= NA, linewidth =0*scale_factor)+

    ggplot2::geom_sf(data = osm_object$buildings.dis, ggplot2::aes(fill = osm_object$buildings.dis$colors), show.legend = F, color= ifelse(!is.null(color$building_border),color$building_border,NA), linewidth =0.001*scale_factor)+
    ggplot2::scale_fill_manual(values=color$palette_building)+

    # remove axes
    ggplot2::theme_void()

    options(warn=0)
    #p$scaling <- scaling

    p$scale_factor <- scale_factor
    return(p)
}

#' Retrieve OSM data for urban environments
#'
#' This function retrieves OSM data with sensible defaults for cities
#'
#' @param lat Latitude WGS84
#' @param lon Longitude WGS84
#' @param x_distance X distance in meters
#' @param y_distance Y distance in meters
#' @param aspect_ratio aspect ratio between width and height
#' @return The shapefiles of buildings, streets, water, beaches, greens, ..
#' @export
get_osmdata <- function(lat, lon, x_distance = NULL, y_distance = NULL, aspect_ratio = NULL, quiet = F) {
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
      stop(cli::cli_abort("get_output_size() must return a vector of length 2 to calculate aspect_ratio."))
    }
  }

  place <- get_border(as.numeric(lat),as.numeric(lon),y_distance,x_distance)
  coords_bbox <- as.numeric(stringr::str_split(osmdata::opq(bbox = place)$bbox,",")[[1]])
  coords_bbox[3]-coords_bbox[1]
  coords_bbox[4]-coords_bbox[2]


  bbox <- sf::st_bbox(c(xmin=coords_bbox[2],xmax=coords_bbox[4],ymin=coords_bbox[1],ymax=coords_bbox[3]), crs=sf::st_crs(4326))

  q.street <- osmdata::opq(bbox = place) |>
    osmdata::add_osm_feature("highway", c("motorway", "primary", "secondary", "tertiary", "unclassified", "residential","living_street","street_lamp", "pedestrian"))

  q1 <- osmdata::opq(bbox = place) |>
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
  if(!quiet) cli::cli_alert_info(paste0("lat:",round(lat,2),", lon:",round(lon,2),", dy:",round(y_distance,2),", dx:", round(x_distance,2)))

  osm <- c()

  if(!quiet) cli::cli_progress_step("Creating street network", spinner = T)

  osm$x <- q.street |> osmdata::osmdata_sf()
  #osm$x$osm_lines <- osm$x$osm_lines |>
  #  dplyr::mutate(length = as.numeric(sf::st_length(osm$x$osm_lines))) |>
  #  dplyr::filter(length >= stats::quantile(length,0.25))
  osm$x$osm_lines$length <- as.numeric(sf::st_length(osm$x$osm_lines))
  length_quantile <- stats::quantile(osm$x$osm_lines$length, 0.25)
  osm$x$osm_lines <- subset(osm$x$osm_lines, length >= length_quantile)

  if(!quiet) cli::cli_progress_step("Constructing buildings", spinner = T)
  osm$x1 <- q1 |> osmdata::osmdata_sf()
  osm$x1$osm_polygons <- osm$x1$osm_polygons |>
  #  (\(x) if(!is.null(osm$x1$osm_polygons$tunnel)) dplyr::filter( osm$x1$osm_polygons, is.na(tunnel) == TRUE) else x)()
    (\(x) if(!is.null(osm$x1$osm_polygons$tunnel)) subset(is.na(tunnel)) else x)()

  if(!is.null(osm$x1$osm_polygons)) osm$x1$osm_polygons$colors <- sample(as.factor(c(1,2,3)) ,dim(osm$x1$osm_polygons)[1], replace = T)
  if(!is.null(osm$x1$osm_multipolygons)) osm$x1$osm_multipolygons$colors <- sample(as.factor(c(1,2,3)) ,dim(osm$x1$osm_multipolygons)[1], replace = T)

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



#' Save map
#'
#' Save map on disk
#'
#' @param plot ggplot object
#' @param filename filename
#' @return NULL
#' @export
save_map <- function(plot, filename) {
  if(!cartographr_env$orientation %in% c('portrait','landscape'))
    stop(cli::cli_abort("Orientation not recognized. Try 'portrait' or 'landscape'"))

  # if scale factors do not match anymore, we have to redraw the plot
  if (plot$scale_factor != cartographr_env$scale_factor) {
    cli::cli_alert_warning("`output_size` was changed after creating the plot, you might get unexpected results.")
  }

  ggplot2::ggsave(plot=plot,
                  filename=filename,
                  device = grDevices::cairo_pdf,
                  width = cartographr_env$output_size[1],
                  height = cartographr_env$output_size[2],
                  units = "mm")
}
