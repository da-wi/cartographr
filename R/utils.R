
#' Adjust display
#'
#' This function adjust displays and should be called last after adding geoms
#'
#' @param osm_object The osm_object with stored bbox
#' @return NULL
#' @export
adjust_display <- function(osm_object) {
  # TODO make this function internal

  return(ggplot2::coord_sf(xlim = c(osm_object$bbox[1]+(osm_object$bbox[3]-osm_object$bbox[1])/20,
                                    osm_object$bbox[3]-(osm_object$bbox[3]-osm_object$bbox[1])/20),
                           ylim = c(osm_object$bbox[2]+(osm_object$bbox[4]-osm_object$bbox[2])/20,
                                    osm_object$bbox[4]-(osm_object$bbox[4]-osm_object$bbox[2])/20)))
}

#' Add acknowledgments
#'
#' Add acknowledgments
#'
#' @return labs
add_acknowledgments <- function() {
  if (cartographr_env$acks) {
    return(ggplot2::labs(caption = "CARTOGRAPHR   ·   OPEN STREET MAP"))
  }
  else {
    return(NULL)
  }
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
  return(data.frame(lat = as.numeric(lat),  long = as.numeric(lon)) |>
           sf::st_as_sf(coords = c("long", "lat"), crs = 4326) |>
           sf::st_transform(crs=7801) |>
           sf::st_buffer(dist = min(y_distance,x_distance)) |>
           sf::st_transform(crs = 4326))
}
