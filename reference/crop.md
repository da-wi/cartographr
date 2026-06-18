# Crop a preprocessed map

This function crops an OpenStreetMap (OSM) object that has been
preprocessed. It supports different types of geometric boundaries such
as rectangles, circles, and hexagons, or a custom boundary provided as
an 'sf' object.

## Usage

``` r
crop(osm, boundary = "rect")
```

## Arguments

- osm:

  A preprocessed OSM object to which the crop will be applied.

- boundary:

  The type of geometric boundary to apply to the OSM data. Can be "rect"
  for a rectangular boundary, "circle" for a circular boundary, "hex"
  for a hexagonal boundary, or an 'sf' object for a custom boundary.
  Default is "rect".

## Value

The OSM object with the specified geometric crop applied.

## Details

If the OSM object has not been preprocessed, the function will call
[`preprocess_map()`](https://da-wi.github.io/cartographr/reference/preprocess_map.md)
to preprocess the data before applying the cropping. The type of
preprocessing applied is stored in the OSM object's metadata.

## Examples

``` r
# \donttest{
data("osm")
# Apply a circular crop
osm_circle_cropped <- osm |> crop(boundary = "circle")
# }
```
