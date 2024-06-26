#' Adjust the viewport for map visualization
#'
#' This internal function adjusts the viewport for map visualization using ggplot2.
#' It modifies the x and y limits based on the bounding box (bbox) of the OpenStreetMap (OSM) object.
#'
#' @param osm_object An OSM object containing a bbox attribute.
#'
#' @return A ggplot2 coordinate (coord_sf) object with adjusted x and y limits.
#'
#' @details
#' The function calculates new x and y limits by adding and subtracting a twentieth of the bbox's width and height
#' from the respective minimum and maximum x and y values. This removes the margin around the map, enhancing visibility.
#'
#' @noRd
#' @keywords internal
adjust_viewport <- function(osm_object) {

  return(ggplot2::coord_sf(xlim = c(osm_object$bbox[1] +(osm_object$bbox[3]-osm_object$bbox[1])/22,
                                    osm_object$bbox[3] -(osm_object$bbox[3]-osm_object$bbox[1])/22),
                           ylim = c(osm_object$bbox[2] +(osm_object$bbox[4]-osm_object$bbox[2])/22,
                                    osm_object$bbox[4] -(osm_object$bbox[4]-osm_object$bbox[2])/22),
                           clip = "on"))
}

#' Add attribution caption to plots
#'
#' This function checks if acknowledgments are enabled in the
#' environment and, if so, adds an attribution caption to the plot using ggplot2.
#' If attributions are not enabled, it returns `NULL`.
#'
#' @return A ggplot2 `labs` object with a caption attribute if acknowledgments
#' are enabled; otherwise, `NULL`.
#' @noRd
#' @keywords internal
add_attribution <- function() {
  if (cartographr_env$attribution) {
    return(ggplot2::labs(caption = "CARTOGRAPHR   |   OPENSTREETMAP"))
  }
  else {
    return(NULL)
  }
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
#' @noRd
#' @keywords internal
get_circle <- function(lat,lon,y_distance,x_distance) {
  return(data.frame(lat = as.numeric(lat),  long = as.numeric(lon)) |>
           sf::st_as_sf(coords = c("long", "lat"), crs = 4326) |>
           sf::st_transform(crs = "+proj=utm +zone=33 +datum=WGS84") |>
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
#' @param orientation X distance in meters
#' @return The hexagon
#' @noRd
#' @keywords internal
get_hexagon <- function(lat, lon, y_distance, x_distance, orientation = "horizontal") {
  if(!(orientation %in% c("vertical","horizontal")))
    stop(cli::cli_abort('{.arg orientation} must be either "vertical" or "horizontal"'))

  n_sides <- 6
  radius <- min(y_distance, x_distance)
  R <- 6378137

  lat_rad <- lat * (pi / 180)
  lon_rad <- lon * (pi / 180)

  # Adjust the starting angle based on the orientation
  start_angle <- ifelse(orientation == "vertical", 0, pi / 2)
  angles <- seq(start_angle, 2 * pi + start_angle, length.out = n_sides + 1)

  lat_hex <- numeric(n_sides)
  lon_hex <- numeric(n_sides)

  # Calculate the coordinates of the hexagon vertices using the haversine formula
  for (i in 1:n_sides) {
    bearing <- angles[i]

    lat_hex[i] <- asin(sin(lat_rad) * cos(radius / R) +
                         cos(lat_rad) * sin(radius / R) * cos(bearing))

    lon_hex[i] <- lon_rad + atan2(sin(bearing) * sin(radius / R) * cos(lat_rad),
                                  cos(radius / R) - sin(lat_rad) * sin(lat_hex[i]))

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

#' Calculate rectengular border given the coordinates
#'
#' This function calculates correct borders
#'
#' @param lat Latitude WSG84
#' @param lon Latitude WSG84
#' @param offlat offset Latitude
#' @param offlon offset Longitude
#' @return The border of the map in Coordinates
#' @noRd
#' @keywords internal
get_border <- function(lat,lon,offlat,offlon) {
  # Earth's radius, sphere
  R = 6378137

  dn = offlat
  de = offlon

  dLat = dn/R
  dLon = de/(R*cos(3.1415*lat/180))

  ymax = lat + dLat * 180/3.1415
  xmax = lon + dLon * 180/3.1415
  ymin = lat - dLat * 180/3.1415
  xmin = lon - dLon * 180/3.1415

  return(c(xmin,ymin,xmax,ymax))
}
