# cartographr

[![R-CMD-check](https://github.com/da-wi/cartographr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/da-wi/cartographr/actions/workflows/R-CMD-check.yaml)
[![codecov test
coverage](https://codecov.io/gh/da-wi/cartographr/branch/develop/graph/badge.svg)](https://app.codecov.io/gh/da-wi/cartographr?branch=develop)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
GPL3](https://img.shields.io/badge/license-GPL3.0-yellow)](https://opensource.org/license/gpl-3-0)
[![](https://cranlogs.r-pkg.org/badges/cartographr)](https://CRAN.R-project.org/package=cartographr)

## Overview

Creating maps from OpenStreetMap data can be complex and time-consuming
due to varying design syntax and the challenge of producing visually
appealing, print-ready maps. Overlaying additional information layers
while maintaining cartographic aesthetics also requires a deep
understanding of spatial data. `cartographr` addresses these issues by
providing a lightweight, user-friendly interface between `osmdata` and
`ggplot2`, streamlining the map-making process and allowing users to
focus on the narrative of their maps. It simplifies the transformation
of geospatial data using simple features (`sf`) geometries into
informative, high-quality visualizations, enhancing the dissemination of
spatial information. Ideal for urban planning, environmental studies, or
public presentations, `cartographr` aims to make map creation
straightforward and effective.

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

Use the
[`get_osmdata()`](https://da-wi.github.io/cartographr/reference/get_osmdata.md)
function to collect OpenStreetMap data. Set the width of your map area
in meters using the `x_distance` parameter. If you leave out the height
(`y_distance`), it will be calculated based on the width and the aspect
ratio of your chosen output size.

``` r

osm <- get_osmdata(48.210, 16.370, x_distance = 1200)
```

The `osm` variable now contains all the geometric shapes (like
buildings, rivers, parks) that will appear on your map. Generate the map
with
[`plot_map()`](https://da-wi.github.io/cartographr/reference/plot_map.md)
and customize its look with themes and color palettes. For instance,
create an infomap of Vienna using
[`theme_infomap()`](https://da-wi.github.io/cartographr/reference/theme_infomap.md)
and choose a color scheme (see
[`get_palette()`](https://da-wi.github.io/cartographr/reference/get_palette.md)).

``` r

plot_vienna <- osm |> plot_map(palette = "serene") +
  theme_infomap() +
  ggplot2::labs(title = "VIENNA")
```

To view your map, simply call

``` r

plot_vienna
```

![](https://github.com/da-wi/cartographr/raw/develop/png/vienna.png)

Finally, save your map as a PDF file, ready for printing.

``` r

save_map(plot = plot_vienna, filename="vienna.pdf")
```

## Palette overview

Several color palettes are provided by the package, however, you can
easily create and use your own palette (see
[`get_palette()`](https://da-wi.github.io/cartographr/reference/get_palette.md)).

``` r

hamburg <- get_osmdata(lat = 53.545, lon = 10.000, x_distance = 1200)

df_pal <- tibble (palettes= c("alphabet", "arctic","autumn", "bw",
                        "evening", "gray", "iberia", "imhof","lines","midnight",
                        "minimal","metropolitan","serene","swiss","tropical"),
                  hamburg = list(hamburg)) |>
  rowwise() |>
  mutate(p = list(hamburg |> plot_map(palettes) + theme_infomap() + labs(title = palettes)))
```

![](https://github.com/da-wi/cartographr/raw/develop/png/plot%20palettes-1.png)![](https://github.com/da-wi/cartographr/raw/develop/png/plot%20palettes-2.png)![](https://github.com/da-wi/cartographr/raw/develop/png/plot%20palettes-3.png)  
![](https://github.com/da-wi/cartographr/raw/develop/png/plot%20palettes-4.png)![](https://github.com/da-wi/cartographr/raw/develop/png/plot%20palettes-5.png)![](https://github.com/da-wi/cartographr/raw/develop/png/plot%20palettes-6.png)  
![](https://github.com/da-wi/cartographr/raw/develop/png/plot%20palettes-7.png)![](https://github.com/da-wi/cartographr/raw/develop/png/plot%20palettes-8.png)![](https://github.com/da-wi/cartographr/raw/develop/png/plot%20palettes-9.png)  
![](https://github.com/da-wi/cartographr/raw/develop/png/plot%20palettes-10.png)![](https://github.com/da-wi/cartographr/raw/develop/png/plot%20palettes-11.png)![](https://github.com/da-wi/cartographr/raw/develop/png/plot%20palettes-12.png)  
![](https://github.com/da-wi/cartographr/raw/develop/png/plot%20palettes-13.png)![](https://github.com/da-wi/cartographr/raw/develop/png/plot%20palettes-14.png)![](https://github.com/da-wi/cartographr/raw/develop/png/plot%20palettes-15.png)

## Data licensing

All data that you access using `cartographr` and, in consequence,
`osmdata` is licensed under OpenStreetMap’s license, the [Open Database
Licence](https://osmfoundation.org/wiki/Licence). Any derived data and
products must also carry the same licence. You should make sure you
understand that licence before publishing any derived datasets.

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/da-wi/cartographr/issues).

## Credits

The development of this project drew inspiration from the
[prettymaps](https://github.com/marceloprates/prettymaps) project.
