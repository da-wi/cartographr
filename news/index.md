# Changelog

## cartographr 0.2.5

- Fixed a bug where get_osmdata() would crash if the Overpass API timed
  out or was unavailable. The function now handles connection failures
  and server-side errors gracefully by issuing a message and returning
  an empty dataset.

## cartographr 0.2.4

CRAN release: 2025-09-18

- Functions using the Overpass API now fail gracefully with an
  informative message; network-dependent tests are skipped on CRAN or
  when the API is unavailable.
- Improved handling of empty or invalid inputs, safer subsetting of OSM
  features, and consistent use of cli::cli_abort().
- Buffers and crop boundaries now use a local metric CRS (UTM/UPS) for
  more accurate results; combined layers keep only polygon geometries.
- [`plot_map()`](https://da-wi.github.io/cartographr/reference/plot_map.md)
  now returns a ggplot2 object; hatch pattern generation and frame
  borders are more reliable.
- Bug fixes: Various small fixes in
  [`crop()`](https://da-wi.github.io/cartographr/reference/crop.md),
  [`get_osmdata()`](https://da-wi.github.io/cartographr/reference/get_osmdata.md),
  and palette handling.

## cartographr 0.2.3

CRAN release: 2025-08-19

- Fix tests for ggplot2 S7 labels; regenerate docs

## cartographr 0.2.2

CRAN release: 2024-06-28

- Released to cran
- Rewrite of README.md to contain examples
- Added images in png folder
- Reorganized vignettes

## cartographr 0.2.1

- New palette and minor bug fixes
- Timeout issues in get_osmdata() fixed

## cartographr 0.2.0

- Added unit tests
- Added crop functionality
- Added hatch functionality
- Added theme options
- Reworked the code for fewer dependencies

## cartographr 0.1

- Initial release to github
