
#' Postcard theme
#'
#' Description of the function
#'
#' @param font The font for output
#' @return The theme
theme_postcard <- function(font = "Poppins") {

  scale_factor <- cartographr_env$scale_factor

  return(ggplot2::theme_void()+
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(hjust=1, size=scale_factor*5*.pt, color="#292e28", family = font),
      panel.border = ggplot2::element_rect(colour = NA, fill=NA),
      panel.background = ggplot2::element_rect(fill=NA, color=NA),
      plot.title = ggplot2::element_text(size=scale_factor*40*.pt,family = font ,face = "bold",hjust = 1,colour = "#292e28",margin=ggplot2::margin(0,0,30*scale_factor,0)),
      plot.caption = ggplot2::element_text(size=scale_factor*8*.pt,family = font ,face = "bold",hjust = 0.5,vjust=1,colour = "#cccccc"),
      plot.subtitle = ggplot2::element_text(size=scale_factor*10*.pt,family = font ,face = "bold",hjust = 1,colour = "#292e28"),
      plot.margin = ggplot2::margin(t = 100*scale_factor, r = 80*scale_factor, b = 80*scale_factor, l = 80*scale_factor, unit = "mm")
    ))
}

#' Postcard theme
#'
#' Description of the function
#'
#' @return The theme
#' @export
theme_postcard_poppins <- function() {
  return(theme_postcard(font = "Poppins"))
}

#' Postcard theme
#'
#' Description of the function
#'
#' @return The theme
#' @export
theme_postcard_anton <- function() {
  return(theme_postcard(font = "Anton"))
}


theme_infomap <- function(font = "Poppins") {
  if (!(font %in% c("Poppins","Anton","Cinzel"))) {
    stop(cli::cli_abort("Font not found."))
  }

  #scale_factor <- mean(scaling[1],scaling[2])
  scale_factor <- cartographr_env$scale_factor

  return(ggplot2::theme_void()+
           ggplot2::theme(
             panel.border = ggplot2::element_rect(colour = NA, fill=NA),
             panel.background = ggplot2::element_rect(fill=NA, color=NA),
             plot.subtitle = ggplot2::element_text(size=scale_factor*10*.pt,family = font,face = "bold",hjust = 1,colour = "#292e28"),
             plot.title = element_text(size = scale_factor*40*.pt,hjust = 0,face = "bold",family=font,colour = "#292e28"),
             plot.margin = margin(t = 40*scale_factor, r = 10*scale_factor, b = 10*scale_factor, l = 10*scale_factor, unit = "mm"),
             plot.caption = element_text(face="plain",size=scale_factor*8*.pt, hjust = 1,family=font,color="#CCCCCC"),
             legend.position = "top",
             legend.title = element_text(size = 10*scale_factor*.pt, family=font, color = "#292e28"),
             legend.text = element_text(size = 10*scale_factor*.pt, family = font, color= "#292e28"),
             legend.justification = c("right","top"),
             legend.margin = margin(-18*scale_factor, 0, 0, 0, unit = "mm")
           ))
}

#' Infomap theme with font Anton
#'
#' Description of the function
#'
#' @param scale_factor The scale factor for output
#' @return The theme
#' @export
theme_infomap_anton <- function() {
  return(theme_infomap(font = "Anton"))
}

#' Infomap theme with font Anton
#'
#' Description of the function
#'
#' @param scale_factor The scale factor for output
#' @return The theme
#' @export
theme_infomap_poppins <- function() {
  return(theme_infomap(font = "Poppins"))
}


#' Adjust display
#'
#' This function adjust displays and should be called last after adding geoms
#'
#' @param osm_object The osm_object with stored bbox
#' @return NULL
#' @export
adjust_display <- function(osm_object) {
  return(ggplot2::coord_sf(xlim = c(osm_object$bbox[1]+(osm_object$bbox[3]-osm_object$bbox[1])/20,
                           osm_object$bbox[3]-(osm_object$bbox[3]-osm_object$bbox[1])/20),
                  ylim = c(osm_object$bbox[2]+(osm_object$bbox[4]-osm_object$bbox[2])/20,
                           osm_object$bbox[4]-(osm_object$bbox[4]-osm_object$bbox[2])/20)))
}

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

  osm_object$water <- dplyr::bind_rows(list(if (!is.null(osm_object$x.water$osm_lines1)) osm_object$x.water$osm_lines1 |> sf::st_make_valid(),
                                            if (!is.null(osm_object$x.water$osm_polygons1)) osm_object$x.water$osm_polygons1  |> sf::st_make_valid(),
                                            if (!is.null(osm_object$x.water$osm_multipolygons1)) osm_object$x.water$osm_multipolygons1 |> sf::st_make_valid(),
                                            if (!is.null(osm_object$x.sea$osm_multipolygons1)) osm_object$x.sea$osm_multipolygons1 |> sf::st_make_valid(),
                                            if (!is.null(osm_object$x.sea$osm_polygons1)) osm_object$x.sea$osm_polygons1 |> sf::st_make_valid()))
  suppressMessages(osm_object$water.dis  <- sf::st_union(osm_object$water[1:dim(osm_object$water)[1],]))

  # buidlings
  osm_object$buildings <- list( osm_object$x1$osm_polygons, osm_object$x1$osm_multipolygons)
  osm_object$buildings <- osm_object$buildings[!sapply(osm_object$buildings,is.null)]
  osm_object$buildings.dis <- NULL
  if (length(osm_object$buildings) > 1) {
    osm_object$buildings.dis <- dplyr::bind_rows(osm_object$buildings)
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
  # scaling factor of output format for adjusting outer margins
  # scale_factor <- mean(scaling[1],scaling[2])
  options(warn=-1)

  osm_object = osm
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
  plot <- plot + adjust_display(osm)

  plot(plot)
}

#' Wrapper function for plot_map
#'
#' This function plots a map
#'
#' @param osm OSM object to plot
#' @param palette Color theme applied to the plot
#' @param scaling scaling applied to the plot
#' @param circle If TRUE draw circle plot
#' @return NULL
#' @export
plot_map <- function(osm, palette = "imhof") {
  # Call the original plot_map function
  plot <- .plot_map(osm, palette) +
    ggnewscale::new_scale_color()+
    ggnewscale::new_scale_fill()

  # Return a custom class object containing the plot and osm data
  structure(plot, class = c("cartographr_ggplot", class(plot)), osm = osm)
}

#' Plot a map
#'
#' This function plots a map
#'
#' @param osm OSM object to plot
#' @param palette Color theme applied to the plot
#' @param circle If TRUE draw circle plot
#' @return NULL
#' @keywords internal
.plot_map = function(osm, palette = "imhof") {

  if (is.null(attr(osm,"preprocessed"))) {
    osm <- preprocess_map(osm)
  }

  color = get_color(palette)

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
      ggplot2::geom_sf(data =osm_object$x.railway$osm_lines |> dplyr::filter(if (!is.null(osm_object$x.railway$osm_lines$tunnel)) is.na(osm_object$x.railway$osm_lines$tunnel) else rep(TRUE,dim(osm_object$x.railway$osm_lines)[1])),
                       linetype = "11",
                       color=color$railway, linewidth = 0.5)} +

    # streets
    ggplot2::geom_sf(data =osm_object$x$osm_lines |> dplyr::filter(highway == "motorway") , color=color$street, linewidth=6*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines |> dplyr::filter(highway == "primary") , color=color$street, linewidth=4*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines |> dplyr::filter(highway == "secondary") , color=color$street, linewidth=4*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines |> dplyr::filter(highway == "tertiary") , color=color$street, linewidth=3*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines |> dplyr::filter(highway == "unclassified") , color=color$street, linewidth=3*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines |> dplyr::filter(highway == "residential") , color=color$street, linewidth=3*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines |> dplyr::filter(highway == "pedestrian") , color=color$street, linewidth=1*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines |> dplyr::filter(highway == "service") , color=color$street, linewidth=1*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines |> dplyr::filter(highway == "living_street") , color=color$street, linewidth=1*scale_factor) +
    {if(!is.null(color$lights)) ggplot2::geom_sf(data = osm_object$x$osm_points, color=color$lights, size=0.2*scale_factor)} +

    # buildings
    #ggplot2::geom_sf(data =osm_object$x1$osm_multipolygons, ggplot2::aes(fill = colors),show.legend = F, color= NA, linewidth = 0*scale_factor)+
    #ggplot2::geom_sf(data =osm_object$x1$osm_polygons, ggplot2::aes(fill = colors), show.legend = F,color= NA, linewidth =0*scale_factor)+
    ggplot2::geom_sf(data = osm_object$buildings.dis, ggplot2::aes(fill = osm_object$buildings.dis$colors), show.legend = F,color= NA, linewidth =0*scale_factor)+
    ggplot2::scale_fill_manual(values=color$palette_building)+

    # remove axes
    ggplot2::theme_void()

    options(warn=0)
    #p$scaling <- scaling

    p$scale_factor <- scale_factor
    return(p)
}



#' Calculate rectengular border given the coordinates
#'
#' This function calculates correct borders
#'
#' @param lat Latitude WSG84
#' @param lon Latitude WSG84
#' @param offlat offset Latitude
#' @param offlon offset Longitude
#' @return The border of the map in Coordinates
#' @export
get_border <- function(lat,lon,offlat,offlon) {
  # Earth's radius, sphere
  R = 6378137

  # offsets in meters
  dn = offlat
  de = offlon

  # Coordinate offsets in radians
  dLat = dn/R
  dLon = de/(R*cos(3.1415*lat/180))

  # Offset, decimal degrees
  ymax = lat + dLat * 180/3.1415
  xmax = lon + dLon * 180/3.1415
  ymin = lat - dLat * 180/3.1415
  xmin = lon - dLon * 180/3.1415

  return(c(xmin,ymin,xmax,ymax))
}


#' Retrieve OSM data for urban environments
#'
#' This function retrieves OSM data with sensible defaults for cities
#'
#' @param lat Latitude WGS84
#' @param lon Longitude WGS84
#' @param y_distance Y distance in meters
#' @param x_distance X distance in meters
#' @return The shapefiles of buildings, streets, water, beaches, greens, ..
#' @export
get_osmdata <- function(lat, lon, y_distance, x_distance, quiet = F) {
#  cli::cli_progress_bar("Tasks", total = 5, type = "tasks")

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
  osm$x$osm_lines <- osm$x$osm_lines |>
    dplyr::mutate(length = as.numeric(sf::st_length(osm$x$osm_lines))) |>
    dplyr::filter(length >= quantile(length,0.25))

  if(!quiet) cli::cli_progress_step("Constructing buildings", spinner = T)
  osm$x1 <- q1 |> osmdata::osmdata_sf()
  osm$x1$osm_polygons <- osm$x1$osm_polygons |>
    (\(x) if(!is.null(osm$x1$osm_polygons$tunnel)) dplyr::filter( osm$x1$osm_polygons, is.na(tunnel) == TRUE) else x)()
    #{if (!is.null(osm$x1$osm_polygons$tunnel)) dplyr::filter( osm$x1$osm_polygons, is.na(tunnel) == TRUE) else .}

  if(!is.null(osm$x1$osm_polygons)) osm$x1$osm_polygons$colors <- sample(as.factor(c(1,2,3)) ,dim(osm$x1$osm_polygons)[1], replace = T)
  if(!is.null(osm$x1$osm_multipolygons)) osm$x1$osm_multipolygons$colors <- sample(as.factor(c(1,2,3)) ,dim(osm$x1$osm_multipolygons)[1], replace = T)

  if(!quiet) cli::cli_progress_step("Filling in water", spinner = T)
  osm$x.water <- q.water |> osmdata::osmdata_sf()
  osm$x.sea <- q.sea |> osmdata::osmdata_sf()

  if(!quiet) cli::cli_progress_step("Planting trees", spinner = T)
  osm$x.green <- q.green |> osmdata::osmdata_sf()


  if(!quiet) cli::cli_progress_step("Creating parking spaces", spinner = T)
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

#' Generate bounding circle
#'
#' This function generates a bounding circle
#'
#' @param lat Latitude WGS84
#' @param lon Longitude WGS84
#' @param y_distance Y distance in meters
#' @param x_distance X distance in meters
#' @return The circle
#' @export
get_circle <- function(lat,lon,y_distance,x_distance) {
  return(tibble::tibble(lat = as.numeric(lat),  long = as.numeric(lon)) |>
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326) |>
    sf::st_transform(crs=7801) |>
    sf::st_buffer(dist = min(y_distance,x_distance)) |>
    sf::st_transform(crs = 4326))
}


#' Save map
#'
#' Save map on disk
#'
#' @param plot ggplot object
#' @param filename filename
#' @param orientation 'portrait' or 'landscape'
#' @return NULL
#' @export
save_map <- function(plot, filename) {
  if(!cartographr_env$orientation %in% c('portrait','landscape'))
    stop(cli::cli_abort("Orientation not recognized. Try 'portrait' or 'landscape'"))

  # if scale factors do not match anymore, we have to redraw the plot
  if (p$scale_factor != cartographr_env$scale_factor) {
    cli::cli_alert_warning("`output_size` was changed after creating the plot, you might get unexpected results.")
  }

  ggplot2::ggsave(plot=plot,
                  filename=filename,
                  device = grDevices::cairo_pdf,
                  width = cartographr_env$output_size[1],
                  height = cartographr_env$output_size[2],
                  units = "mm")
}

#' Create color theme
#'
#' This function creates a color theme
#'
#' @param palette The color palette. Control appearance of street lamps by setting a color (= shown) or NULL (leave blank, = not shown)
#' @return A color palette for plot_map()
#' @export
get_color = function(palette) {
  color <- c()

  color$name <- palette

  if (palette == "macau") {
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

  if (palette == "barcelona") {
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

  # NIGHT
  if (palette == "night") {
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

  # NIGHT arkham
  if (palette == "arkham") {
    color$palette_building = c("#012840","#012033", "#012840")
    color$water <- "#012033"
    color$green <- "#035080"
    color$railway <- "#025E73"
    color$lights <- "#7ADB9B" #"#04D98B" # "#F28B0C" # "#f7f7c8"
    color$beach <- "#025E73"
    color$parking <- "#025E73"
    color$street <- "#025E73"
    color$background <- "#025E73"
    color$hatched <- FALSE
  }

  # IMHOF
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

  # BW
  if (palette == "bw") {
    color$palette_building = c("#FFFFFF","#FFFFFF", "#FFFFFF")
    color$railway <- "#292e28"
    color$green <- "#FFFFFF"
    color$water <- "#292e28" #"#97B2B9"
    color$background <- "#ffffff" # "#D3CCAF" #"#192058"
    color$street <- "#292e28"
    color$beach <- "#ffffff"
    color$parking <- "#Ffffff"
    color$hatched <- FALSE
  }

  if (palette == "bwinv") {
    color$palette_building = c("#292e28","#292e28", "#292e28")
    #color$palette_building = c(NA,NA, NA)
    color$railway <- "#faf5eb"#FFFFFF"
    color$green <- "#faf5eb"
    color$water <- "#faf5eb" #"#97B2B9"
    color$background <- "#292e28" # "#D3CCAF" #"#192058"
    color$street <- "#faf5eb"
    color$beach <- "#faf5eb"
    color$parking <- "#faf5eb"
    color$hatched <- FALSE
  }

  # ABC
  if (palette == "amsterdam") {
    color$palette_building = c("#ED6012","#046D69", "#383922")
    color$railway <- "#1b1b1b"
    color$green <- "#7C6236"
    color$water <- "#1b1b1b" #"#97B2B9"
    color$background <- "#E5DBA9" # "#D3CCAF" #"#192058"
    color$street <- "#E5DBA9"
    color$beach <- "#E5DBA9"
    color$parking <- "#E5DBA9"
    color$hatched <- FALSE
  }



  if (palette == "berlin") {
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

  if (palette == "wesanderson") {
    color$palette_building = c("#b79ea8","#a393a2", "#8a7e8c")
    color$railway <- "#3b3633"
    color$green <- "#e98c8a"
    color$water <- "#ddeef2" #"#97B2B9"
    color$background <- "#fbe4e4" # "#D3CCAF" #"#192058"
    color$street <- "#fbe4e4"
    color$beach <- "#e98c8a"
    color$parking <- "#fbe4e4"
    color$hatched <- T
  }

  if (palette == "modern") {
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

  if (palette == "bejing") {
    color$palette_building <- c("#94adaa","#a0b3ad","#b4bbad")
    color$water <- c("#353770")
    color$street <- c("#4a3e88")
    color$green <- c("#f5e0b3")
    color$background <- c("#f5e0b3")
    color$parking <- c("#f5e0b3")
    color$railway <- c("#c2c2bf")
    color$hatched <- FALSE
    color$beach <- "#ded2c4"
    color$parking <- "#fcf5d7"
  }

  if (palette == "darkmode") {
    color$palette_building <- c("#1d2436","#283349","#1d2436")
    color$water <- c("#06070b")
    color$street <- c("#404b5d")
    color$green <- c("#213d40")
    color$background <- c("#283349")
    color$parking <- c("#404b5d")
    color$railway <- c("#c2c2bf")
    color$hatched <- FALSE
    color$beach <- "#283349"
    color$parking <- "#283349"
  }

  return(color)
}
