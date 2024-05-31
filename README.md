<!-- README.md is generated from README.Rmd. Please edit that file -->

# cartographr <a href='https://da-wi.github.io/cartographr'><img src="man/figures/logo.png" align="right" height="139" /></a>

## Overview

*cartographr* is designed to streamline the process of creating visually
appealing, print-ready maps from OpenStreetMap data. It serves as a
powerful tool for users looking to represent base maps and overlay
additional information layers, much like the familiar ggplot2 syntax.
The package simplifies the complex task of map-making by handling the
intricacies of spatial data and cartographic aesthetics, allowing users
to focus on the storytelling aspect of their maps. Whether for urban
planning, environmental studies, or public presentations, this package
offers a convenient and flexible way to transform raw geospatial data
into informative, publication-quality visualizations, enhancing the
communication of spatial information to a broad audience.

## Installation

Simply install from github.

``` r
devtools::install_github("da-wi/cartographr")
```

## Usage

The basic workflow consists of

1.  Retrieving openstreet map data for a given area:

``` r
osm <- get_osmdata(lat =  36.188, lon = -115.176, x_distance = 1000)
```

2.  Plot the map

``` r
osm |> plot_map()
```

3.  Customize your map and explore visualization options

With the `plot_map()` function, you’re empowered to enhance your maps
with additional layers of data using `ggplot2`, tailoring the final
product to your specific needs. The function’s flexibility allows you to
select from a variety of preset color palettes and themes to best suit
your project’s requirements, or you can craft a unique aesthetic by
creating your own. This adaptability makes it an ideal choice for anyone
looking to present information on a map in a way that is both
informative and visually engaging.

``` r
osm |> plot_map(palette = "arctic") +
  geom_sf(data = location_tracking, aes(color = species)) +
  theme_infomap()
```

## Data licensing

All data that you access using `cartographr` and, in consequence,
`osmdata` is licensed under OpenStreetMap’s license, the
<a href="https://wiki.osmfoundation.org/wiki/Licence">Open Database
Licence</a>. Any derived data and products must also carry the same
licence. You should make sure you understand that licence before
publishing any derived datasets.

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
<a href="https://github.com/da-wi/cartographr/issues">GitHub</a>.
