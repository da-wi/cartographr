#' Manhattan crime dataset
#'
#' This dataset includes all valid felony, misdemeanor, and violation crimes reported to the New York City Police Department (NYPD) for Manhattan from 2004.
#' \url{https://catalog.data.gov/}.
"crime"

#' SoHo Boundary Simple Features Vector
#'
#' This dataset represents the Simple Features vector for the MN24 region of New York,
#' specifically covering the SoHo neighborhood. It includes spatial boundaries and
#' other relevant geographical attributes.
#'
#' @format An object of class \code{sf} (inherits from \code{data.frame}),
#' representing the SoHo boundary with its spatial attributes.
#' @source https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
#' @examples
#' data(soho_boundary)
#' plot(soho_boundary$geometry)
"soho_boundary"

#' OSM SoHo New York simple features
#'
#' This dataset contains the Simple Features (sf) representation for Soho, New York.
#' It includes various geographical and spatial attributes relevant to the area.
#'
#' @format An named list of objects of class \code{sf}
#' with rows and columns corresponding to the features and their attributes.
#' @source OpenStreetMap
#' @examples
#' data("osm")
#' osm |> plot_map()
"osm"
