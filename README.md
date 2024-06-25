<!-- README.md is generated from README.Rmd. Please edit that file -->

# cartographr <a href='https://da-wi.github.io/cartographr'><img src="man/figures/logo.png" align="right" height="139" /></a>

[![R-CMD-check](https://github.com/da-wi/cartographr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/da-wi/cartographr/actions/workflows/R-CMD-check.yaml)
[![codecov test
coverage](https://codecov.io/gh/da-wi/cartographr/branch/develop/graph/badge.svg)](https://app.codecov.io/gh/da-wi/cartographr?branch=develop)

## Overview

Creating maps from OpenStreetMap data can be a complex and
time-consuming process. The syntax for designing maps often lacks
intuitiveness and can vary greatly, making it challenging for users to
produce visually appealing, print-ready maps. Additionally, overlaying
additional information layers while maintaining cartographic aesthetics
requires a deep understanding of spatial data, which can detract from
the storytelling aspect of map creation. `cartographr` addresses these
issues by providing a lightweight and user-friendly interface between
`osmdata` and `ggplot2`, streamlining the map-making process and
allowing users to concentrate on the narrative conveyed by their maps.
It offers a practical and adaptable solution for transforming geospatial
data using simple features (`sf`) geometries into informative,
high-quality visualizations, thereby improving the dissemination of
spatial information. Whether you’re involved in urban planning,
environmental studies, or need to create impactful public presentations,
`cartographr` is designed to make your work as straightforward and
effective as possible.

## Installation

Simply install from github.

``` r
# Install the latest version from github
devtools::install_github("da-wi/cartographr")

# Alternatively, install from CRAN
install.packages("cartographr")
```

## Usage

Begin by determining the central point of your map using the
[WGS84](https://de.wikipedia.org/wiki/World_Geodetic_System_1984)
coordinates. For our example, we’ll use Vienna’s center with a latitude
of `48.210` and a longitude of `16.370`. You can easily find these
coordinates online.

Decide on the size of the printed map, such as A4. This will help scale
the text and lines on the map proportionally, no matter the print size.

``` r
set_output_size(c(300,300))
```

Use the `get_osmdata()` function to collect OpenStreetMap data. Set the
width of your map area in meters using the `x_distance` parameter. If
you leave out the height (`y_distance`), it will be calculated based on
the width and the aspect ratio of your chosen output size.

``` r
osm <- get_osmdata(48.210, 16.370, x_distance = 1200)
```

The `osm` variable now contains all the geometric shapes (like
buildings, rivers, parks) that will appear on your map. Generate the map
with `plot_map()` and customize its look with themes and color palettes.
For instance, create an infomap of Vienna using `theme_infomap()` and
choose a color scheme (see `get_palette()`).

``` r
plot_vienna <- osm |> plot_map(palette = "serene") +
  theme_infomap() +
  ggplot2::labs(title = "VIENNA")
```

To view your map, simply call

``` r
plot_vienna
```

<img src='https://github.com/da-wi/cartographr/raw/develop/png/vienna.png' width='100%'>

Finally, save your map as a PDF file, ready for printing.

``` r
save_map(plot = plot_vienna, filename="vienna.pdf")
```

## Palette overview

Several color palettes are provided by the package, however, you can
easily create and use your own palette (see `get_palette()`).

``` r
hamburg <- get_osmdata(lat = 53.545, lon = 10.000, x_distance = 1200)

df_pal <- tibble (palettes= c("alphabet", "arctic","autumn", "bw",
                        "evening", "gray", "iberia", "imhof","lines","midnight",
                        "minimal","metropolitan","serene","swiss","tropical"),
                  hamburg = list(hamburg)) |>
  rowwise() |>
  mutate(p = list(hamburg |> plot_map(palettes) + theme_infomap() + labs(title = palettes)))
```

<p align="center">
<img src='https://github.com/da-wi/cartographr/raw/develop/png/plot palettes-1.png' width='30%'>
<img src='https://github.com/da-wi/cartographr/raw/develop/png/plot palettes-2.png' width='30%'>
<img src='https://github.com/da-wi/cartographr/raw/develop/png/plot palettes-3.png' width='30%'>
<br>
<img src='https://github.com/da-wi/cartographr/raw/develop/png/plot palettes-4.png' width='30%'>
<img src='https://github.com/da-wi/cartographr/raw/develop/png/plot palettes-5.png' width='30%'>
<img src='https://github.com/da-wi/cartographr/raw/develop/png/plot palettes-6.png' width='30%'>
<br>
<img src='https://github.com/da-wi/cartographr/raw/develop/png/plot palettes-7.png' width='30%'>
<img src='https://github.com/da-wi/cartographr/raw/develop/png/plot palettes-8.png' width='30%'>
<img src='https://github.com/da-wi/cartographr/raw/develop/png/plot palettes-9.png' width='30%'>
<br>
<img src='https://github.com/da-wi/cartographr/raw/develop/png/plot palettes-10.png' width='30%'>
<img src='https://github.com/da-wi/cartographr/raw/develop/png/plot palettes-11.png' width='30%'>
<img src='https://github.com/da-wi/cartographr/raw/develop/png/plot palettes-12.png' width='30%'>
<br>
<img src='https://github.com/da-wi/cartographr/raw/develop/png/plot palettes-13.png' width='30%'>
<img src='https://github.com/da-wi/cartographr/raw/develop/png/plot palettes-14.png' width='30%'>
<img src='https://github.com/da-wi/cartographr/raw/develop/png/plot palettes-15.png' width='30%'>
</p>

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

## Credits

The development of this project drew inspiration from the
[prettymaps](https://github.com/marceloprates/prettymaps) project.
