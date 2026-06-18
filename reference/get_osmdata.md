# Retrieve OpenStreetMap Data

This function retrieves OpenStreetMap (OSM) data based on geographic
coordinates or a bounding box. It allows for the specification of
distances and aspect ratios to define the area of interest. The function
can return data in simple features (sf) format and has options to
operate quietly.

## Usage

``` r
get_osmdata(
  lat = NULL,
  lon = NULL,
  x_distance = NULL,
  y_distance = NULL,
  aspect_ratio = NULL,
  bbox = NULL,
  sf = NULL,
  quiet = TRUE,
  keep = TRUE
)
```

## Arguments

- lat:

  Latitude of the center point (optional if bbox or sf is provided).

- lon:

  Longitude of the center point (optional if bbox or sf is provided).

- x_distance:

  Distance in the x-direction from the center point (optional).

- y_distance:

  Distance in the y-direction from the center point (optional).

- aspect_ratio:

  Aspect ratio of the x and y distances (optional).

- bbox:

  A bounding box to define the area of interest (optional if lat, lon,
  and distances are provided).

- sf:

  An sf object to define the area of interest (optional if bbox is
  provided).

- quiet:

  Logical flag to suppress progress messages.

- keep:

  Logical flag if additional OSM data should be kept.

## Value

A list containing various elements of the OSM data, including street
networks, buildings, water bodies, green areas, beaches, parking areas,
railways, and the bounding box of the retrieved area.

## Details

The function performs checks to ensure the correct combination of
parameters is provided. It calculates the bounding box if not provided
and retrieves various OSM features within the specified area.

## Exported Features

The following table lists the OSM features that are retrieved by the
function:

|  |  |
|----|----|
| Feature Type | OSM Tags |
| highway | motorway, motorway_link, trunk, trunk_link, primary, secondary, tertiary, |
|  | unclassified, residential, living_street, street_lamp, pedestrian, track, path, steps |
| water | \* |
| building | \* |
| natural | beach, water, strait, bay, island, wood |
| amenity | parking |
| man_made | pier |
| railway | rail |
| place | sea, ocean |
| boundary | maritime |
| waterway | stream |
| landuse | forest, farmland, grass, orchard, allotments, recreation_ground, vineyard, cemetery, meadow |
| leisure | swimming_pool, pitch, nature_reserve, garden, park |
| natural | bay, island, wood |

Note: \* all tags are retrieved

## Examples

``` r
# \donttest{
osm_data <- get_osmdata(lat=44.568611, lon=15.331389, x_distance=100)
#> Overpass API query failed or is unavailable: arguments imply differing number of rows: 60, 0
# }
```
