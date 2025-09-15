# cartographr 0.2.4

* Functions using the Overpass API now fail gracefully with an informative message; network-dependent tests are skipped on CRAN or when the API is unavailable.
* Improved handling of empty or invalid inputs, safer subsetting of OSM features, and consistent use of cli::cli_abort().
* Buffers and crop boundaries now use a local metric CRS (UTM/UPS) for more accurate results; combined layers keep only polygon geometries.
* `plot_map()` now returns a ggplot2 object; hatch pattern generation and frame borders are more reliable.
* Bug fixes: Various small fixes in `crop()`, `get_osmdata()`, and palette handling.

# cartographr 0.2.3

* Fix tests for ggplot2 S7 labels; regenerate docs

# cartographr 0.2.2

* Released to cran
* Rewrite of README.md to contain examples
* Added images in png folder
* Reorganized vignettes

# cartographr 0.2.1

* New palette and minor bug fixes
* Timeout issues in get\_osmdata() fixed

# cartographr 0.2.0

* Added unit tests
* Added crop functionality
* Added hatch functionality
* Added theme options
* Reworked the code for fewer dependencies

# cartographr 0.1

* Initial release to github
