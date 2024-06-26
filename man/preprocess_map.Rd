% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cartographr.R
\name{preprocess_map}
\alias{preprocess_map}
\title{Preprocess OSM Data}
\usage{
preprocess_map(osm)
}
\arguments{
\item{osm}{A list containing OSM data elements.}
}
\value{
A list with preprocessed OSM data elements, including streets, railways, buildings,
water bodies, green areas, beaches, and parking areas, each as separate list elements.
Also includes a combined multipolygon for water, buildings, and green areas for plotting.
}
\description{
This function preprocesses OpenStreetMap (OSM) data for further analysis and visualization.
It filters and organizes data related to streets, railways, buildings, water bodies, green areas,
beaches, and parking areas.
}
\details{
The function performs the following steps:
\itemize{
\item Filters streets based on specified highway types.
\item Filters railway lines.
\item Filters building polygons and multipolygons.
\item Filters water bodies and sea areas using multiple criteria.
\item Filters green areas based on land use and natural features.
\item Filters beach areas based on natural features.
\item Filters parking areas based on amenities, highways, and man-made features.
\item Combines multiple polygons into a single multipolygon for water, buildings, and green areas.
}

It returns the original OSM list with additional elements for each category of data and
combined multipolygons for easy plotting.
}
\examples{
data("osm")
preprocessed_osm <- osm |> preprocess_map()
}
