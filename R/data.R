#' Manhattan crime dataset
#'
#' This dataset encompasses all reported felony, misdemeanor, and violation crimes as recorded by the New York City Police Department (NYPD) for the borough of Manhattan, starting from the year 2004.
#' The data is sourced from the public domain and is available for analysis and research purposes. It provides a comprehensive overview of crime patterns and can be utilized for developing crime prevention strategies, conducting sociological research, and enhancing public awareness.
#'
#' @format A data frame with columns representing various attributes of crimes such as type, location, date, and time.
#' @source New York City Police Department (NYPD) \url{https://catalog.data.gov/}
"crime"

#' SoHo Boundary Simple Features Vector
#'
#' This dataset represents the Simple Features vector for the MN24 region of New York,
#' specifically covering the SoHo neighborhood. It includes spatial boundaries and
#' other relevant geographical attributes.
#'
#' @format An object of class \code{sf} (inherits from \code{data.frame}),
#' representing the SoHo boundary with its spatial attributes.
#' @source \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}
#' @examples
#' data(soho_boundary)
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
"osm"
