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
issues by providing a user-friendly interface derived from `ggplot2`,
streamlining the map-making process and allowing users to concentrate on
the narrative conveyed by their maps. It offers a practical and
adaptable solution for transforming geospatial data using simple
features (`sf`) geometries into informative, high-quality
visualizations, thereby improving the dissemination of spatial
information. Whether you’re involved in urban planning, environmental
studies, or need to create impactful public presentations, `cartographr`
is designed to make your work as straightforward and effective as
possible.

## Installation

Simply install from github.

``` r
devtools::install_github("da-wi/cartographr")
```

## Usage

``` r
get_osmdata(lat = 50, lon = 10, x_distance = 1000) |> plot_map()
```

For a comprehensive walkthrough of the package’s features, please refer
to the [introductory vignette](articles/cartographr.html).

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
