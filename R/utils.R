
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

#' Generate bounding hexagon
#'
#' This function generates a bounding hexagon
#'
#' @param lat Latitude WGS84
#' @param lon Longitude WGS84
#' @param y_distance Y distance in meters
#' @param x_distance X distance in meters
#' @return The hexagon
#' @export
get_hexagon <- function(lat, lon, y_distance, x_distance) {
  n_sides <- 6
  radius <- min(y_distance, x_distance)
  earth_radius <- 6371000

  lat_rad <- lat * (pi / 180)
  lon_rad <- lon * (pi / 180)

  angles <- seq(0, 2 * pi, length.out = n_sides + 1)

  lat_hex <- numeric(n_sides)
  lon_hex <- numeric(n_sides)

  # Calculate the coordinates of the hexagon vertices using the haversine formula
  for (i in 1:n_sides) {
    bearing <- angles[i]

    lat_hex[i] <- asin(sin(lat_rad) * cos(radius / earth_radius) +
                         cos(lat_rad) * sin(radius / earth_radius) * cos(bearing))

    lon_hex[i] <- lon_rad + atan2(sin(bearing) * sin(radius / earth_radius) * cos(lat_rad),
                                  cos(radius / earth_radius) - sin(lat_rad) * sin(lat_hex[i]))

    # Convert the radians back to degrees
    lat_hex[i] <- lat_hex[i] * (180 / pi)
    lon_hex[i] <- lon_hex[i] * (180 / pi)
  }

  # Create a data frame of the hexagon vertices
  hex_coords <- data.frame(lat = lat_hex, lon = lon_hex)
  #print(hex_coords)
  # Convert the hexagon vertices into an sf object and set the CRS
  #return(hex_coords)
  hex_sf <- sf::st_as_sf(hex_coords, coords = c("lon", "lat"), crs = 4326)

  # Combine the geometries (vertices) into a single polygon
  combined_polygon <- sf::st_combine(hex_sf$geometry)

  # Cast the combined geometry to a POLYGON
  hex_polygon <- sf::st_cast(combined_polygon, "POLYGON")

  return(hex_polygon)
}
