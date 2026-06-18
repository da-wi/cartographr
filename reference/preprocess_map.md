# Preprocess OSM Data

This function preprocesses OpenStreetMap (OSM) data for further analysis
and visualization. It filters and organizes data related to streets,
railways, buildings, water bodies, green areas, beaches, and parking
areas.

## Usage

``` r
preprocess_map(osm)
```

## Arguments

- osm:

  A list containing OSM data elements.

## Value

A list with preprocessed OSM data elements, including streets, railways,
buildings, water bodies, green areas, beaches, and parking areas, each
as separate list elements. Also includes a combined multipolygon for
water, buildings, and green areas for plotting.

## Details

The function performs the following steps:

- Filters streets based on specified highway types.

- Filters railway lines.

- Filters building polygons and multipolygons.

- Filters water bodies and sea areas using multiple criteria.

- Filters green areas based on land use and natural features.

- Filters beach areas based on natural features.

- Filters parking areas based on amenities, highways, and man-made
  features.

- Combines multiple polygons into a single multipolygon for water,
  buildings, and green areas.

It returns the original OSM list with additional elements for each
category of data and combined multipolygons for easy plotting.

## Examples

``` r
data("osm")
preprocessed_osm <- osm |> preprocess_map()
```
