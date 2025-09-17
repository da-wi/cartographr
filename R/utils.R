#' Adjust the viewport for map visualization
#'
#' This function adjusts the viewport for map visualization using ggplot2.
#' It modifies the x and y limits based on the bounding box (bbox) of the OpenStreetMap (OSM) object.
#'
#' @param osm_object An OSM object containing a bbox attribute.
#'
#' @return A ggplot2 coordinate (coord_sf) object with adjusted x and y limits.
#'
#' @details
#' The function calculates new x and y limits by adding and subtracting a twentieth of the bbox's width and height
#' from the respective minimum and maximum x and y values. This removes the margin around the map, enhancing visibility.
#' @noRd
#' @keywords internal
adjust_viewport <- function(osm_object) {
  # return(ggplot2::coord_sf(xlim = c(osm_object$bbox[1] +(osm_object$bbox[3]-osm_object$bbox[1])/22,
  #                                   osm_object$bbox[3] -(osm_object$bbox[3]-osm_object$bbox[1])/22),
  #                          ylim = c(osm_object$bbox[2] +(osm_object$bbox[4]-osm_object$bbox[2])/22,
  #                                   osm_object$bbox[4] -(osm_object$bbox[4]-osm_object$bbox[2])/22),
  #                          clip = "off", expand = FALSE))
  return(ggplot2::coord_sf(xlim = c(osm_object$bbox[1] ,
                                    osm_object$bbox[3] ),
                            ylim = c(osm_object$bbox[2] ,
                                     osm_object$bbox[4]) ,
                            clip = "off", expand = FALSE))

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

#' Choose local metric CRS
#'
#' This function returns the appropriate CRS
#'
#' @return A CRS
#' @noRd
#' @keywords internal
choose_local_metric_crs <- function(lat, lon) {
  if (lat <= -80 || lat >= 84) {
    return(sf::st_crs(if (lat >= 0) 32661 else 32761))  # UPS
  }
  zone <- floor((lon + 180) / 6) + 1L
  epsg <- if (lat >= 0) 32600 + zone else 32700 + zone  # UTM N/S
  sf::st_crs(epsg)
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
get_circle <- function(lat, lon, y_distance, x_distance) {
  r <- min(as.numeric(y_distance), as.numeric(x_distance))
  stopifnot(is.finite(r), r > 0)
  crs_metric <- choose_local_metric_crs(lat, lon)

  sf::st_as_sf(data.frame(long = as.numeric(lon), lat = as.numeric(lat)),
               coords = c("long", "lat"), crs = 4326) |>
    sf::st_transform(crs_metric) |>
    sf::st_buffer(dist = r) |>
    sf::st_transform(4326)
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
    cli::cli_abort('{.arg orientation} must be either "vertical" or "horizontal"')

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
  dLon = de/(R*cos(pi*lat/180))

  ymax = lat + dLat * 180/3.1415
  xmax = lon + dLon * 180/3.1415
  ymin = lat - dLat * 180/3.1415
  xmin = lon - dLon * 180/3.1415

  return(c(xmin,ymin,xmax,ymax))
}

#' Construct an empty osmdata sf container
#' @param crs EPSG code for the geometry columns (default 4326)
#' @return An object of class c("osmdata", "osmdata_sf") with empty layers
#' @noRd
#' @keywords internal
empty_osmdata_sf <- function(crs = 4326) {
  empty_sf <- function() sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(crs)))[0, ]

  out <- list(
    bbox               = sf::st_bbox(c(xmin = NA_real_, ymin = NA_real_,
                                       xmax = NA_real_, ymax = NA_real_),
                                     crs = sf::st_crs(crs)),
    overpass_call      = list(url = NA_character_),
    osm_points         = empty_sf(),
    osm_lines          = empty_sf(),
    osm_polygons       = empty_sf(),
    osm_multilines     = empty_sf(),
    osm_multipolygons  = empty_sf()
  )
  class(out) <- c("osmdata", "osmdata_sf")
  out
}

# Skip online tests on CRAN or when Overpass is unreachable
#' @noRd
#' @keywords internal
skip_if_no_overpass <- function() {
  testthat::skip_on_cran()              # never hit the network on CRAN
  testthat::skip_on_ci()

  if (!curl::has_internet()) {
    testthat::skip("No internet; skipping online tests.")
  }
  ok <- tryCatch({
    httr2::request("https://overpass-api.de/api/status") |>
      httr2::req_timeout(5) |>
      httr2::req_perform()
    TRUE
  }, error = function(e) FALSE)
  if (!ok) testthat::skip("Overpass API not reachable; skipping.")
}

#' Build a border ring around crop_extent
#' @keywords internal
.make_border_ring <- function(crop_extent, lat = NULL, lon = NULL,
                              border_width = 0.001,
                              width_unit = c("rel", "m", "mm")) {
  width_unit <- match.arg(width_unit)
  if (is.null(crop_extent)) return(NULL)

  # Derive center if lat/lon not provided
  if (is.null(lat) || is.null(lon)) {
    ctr <- try(suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_as_sfc(sf::st_bbox(crop_extent)))[1])), silent = TRUE)
    if (!inherits(ctr, "try-error")) {
      lon <- ctr[1]; lat <- ctr[2]
    } else {
      lon <- 0; lat <- 0  # harmless fallback
    }
  }

  # Choose a local metric CRS (UTM/UPS)
  crs_metric <- choose_local_metric_crs(lat, lon)

  # Project to metric, compute diag length in meters
  ce_g <- sf::st_geometry(crop_extent)           # geometry only
  ce_p <- sf::st_transform(ce_g, crs_metric)
  bb   <- sf::st_bbox(ce_p)
  diag_m <- sqrt((bb$xmax - bb$xmin)^2 + (bb$ymax - bb$ymin)^2)

  # Compute buffer in meters
  width_m <- switch(
    width_unit,
    rel = as.numeric(border_width) * diag_m,
    m   = as.numeric(border_width),
    mm  = {
      # convert mm on output device to meters on map
      out_mm <- cartographr_env$output_size
      # pick the larger dimension to be conservative
      plot_mm <- max(out_mm)
      # map-width in meters:
      map_w_m <- (bb$xmax - bb$xmin)
      (as.numeric(border_width) / plot_mm) * map_w_m
    }
  )
  if (!is.finite(width_m) || width_m <= 0) return(NULL)

  # Build ring and return geometry-only sf
  buffered <- suppressWarnings(sf::st_buffer(ce_p, dist = width_m))
  ring_p   <- suppressWarnings(sf::st_difference(buffered, ce_p))
  if (sf::st_is_empty(ring_p)) return(NULL)

  ring_ll  <- sf::st_transform(ring_p, sf::st_crs(crop_extent))
  ring_ll  <- suppressWarnings(sf::st_cast(ring_ll, "MULTIPOLYGON", warn = FALSE))
  ring_sf  <- sf::st_sf(geometry = sf::st_geometry(ring_ll))
  sf::st_agr(ring_sf) <- "constant"
  ring_sf
}

