# Create a palette for maps

This function creates a color theme to be used with
[`plot_map()`](https://da-wi.github.io/cartographr/reference/plot_map.md).
It can accept a predefined palette name or a custom palette provided as
a named list.

## Usage

``` r
get_palette(palette)
```

## Arguments

- palette:

  The color palette to use. Can be one of "alphabet", "arctic",
  "autumn", "autumn-muted", "bw", "desert", "evening", "gray", "iberia",
  "imhof", "lines", "metropolitan", "midnight", "minimal", "swiss",
  "tropical", or a named list for a custom palette. If a named list is
  provided, it should contain color hex codes for each map element. If
  `NULL` or an unrecognized name is provided, the function will throw an
  error.

## Value

A list containing color settings for the map elements.

## Details

The color moods for the predefined palettes are described as follows:

- **Alphabet**: A modern palette with a straightforward aesthetic.

- **Arctic**: A palette that reflects the clear and bright qualities of
  Arctic landscapes.

- **Autumn**: A palette with the warm and varied hues typical of the
  fall season.

- **BW**: A classic black and white palette with a hint of warmth for a
  traditional feel.

- **Evening**: A palette that embodies the quiet and contemplative
  nature of dusk.

- **Gray**: A balanced palette that provides a composed and refined
  look.

- **Iberia**: A palette that reflects the warm and diverse tones
  associated with the Iberian landscape.

- **Imhof**: A palette with natural and subdued tones, inspired by the
  work of cartographer Eduard Imhof.

- **Lines (BW)**: A contrasting black and white palette for a clear and
  defined appearance.

- **Metropolitan**: A palette with understated tones that suggest urban
  sophistication.

- **Midnight**: A palette that conveys the depth and tranquility of the
  night.

- **Minimal**: A palette focused on minimalism, utilizing primarily
  whites and light grays.

- **Serene**: A palette that embodies peace and simplicity, utilizing a
  soft color scheme with gentle contrasts.

- **Swiss**: A palette that emphasizes cleanliness and precision,
  reminiscent of Swiss design.

- **Tropical**: A lively palette with the bright and bold colors
  characteristic of tropical areas.

In addition, you can customize other settings:

- `border_color`: The color of the borders, set to a dark shade
  "#121212".

- `border_width`: The width of the borders, set to a very fine line of
  0.001 units.

- `linewidth_buildings`: The line width for building outlines, set to
  0.05 units.

- `linewidth_motorway`, `linewidth_trunk`, `linewidth_primary`,
  `linewidth_secondary`, `linewidth_tertiary`, `linewidth_unclassified`,
  `linewidth_residential`: The line widths for various types of roads,
  ranging from 6 units for motorways to 1 unit for pedestrian paths.

- `size_streetlamp`: The size representation for streetlamps, set to 0.2
  units.

- `hatch_*`: A series of settings for hatching patterns, which can be
  applied to water, buildings, and green spaces. These include toggles
  for hatching (`hatch_water`, `hatch_buildings`, `hatch_green`), the
  number of points or lines (`hatch_*_npoints`, `hatch_*_nlines`), the
  type of hatching pattern (`hatch_*_type`), the size of the hatching
  elements (`hatch_*_size`), and the transparency level
  (`hatch_*_alpha`).

These settings allow for a high degree of customization when creating
maps, providing users with the ability to fine-tune the appearance of
their map elements according to their specific needs or preferences.

## Examples

``` r
# Use predefined palette
get_palette("imhof")
#> $name
#> [1] "imhof"
#> 
#> $border_color
#> [1] "#121212"
#> 
#> $border_width
#> [1] 0.001
#> 
#> $linewidth_buildings
#> [1] 0.05
#> 
#> $linewidth_motorway
#> [1] 6
#> 
#> $linewidth_trunk
#> [1] 6
#> 
#> $linewidth_primary
#> [1] 4
#> 
#> $linewidth_secondary
#> [1] 4
#> 
#> $linewidth_tertiary
#> [1] 3
#> 
#> $linewidth_unclassified
#> [1] 3
#> 
#> $linewidth_residential
#> [1] 3
#> 
#> $linewidth_pedestrian
#> [1] 1
#> 
#> $linewidth_service
#> [1] 1
#> 
#> $linewidth_living_street
#> [1] 1
#> 
#> $size_streetlamp
#> [1] 0.2
#> 
#> $hatch_water
#> [1] FALSE
#> 
#> $hatch_water_npoints
#> [1] 200
#> 
#> $hatch_water_nlines
#> [1] 100
#> 
#> $hatch_water_type
#> [1] "points"
#> 
#> $hatch_water_size
#> [1] 1
#> 
#> $hatch_water_alpha
#> [1] 0.1
#> 
#> $hatch_buildings
#> [1] FALSE
#> 
#> $hatch_buildings_npoints
#> [1] 200
#> 
#> $hatch_buildings_nlines
#> [1] 100
#> 
#> $hatch_buildings_type
#> [1] "points"
#> 
#> $hatch_buildings_size
#> [1] 1
#> 
#> $hatch_buildings_alpha
#> [1] 0.1
#> 
#> $hatch_green
#> [1] FALSE
#> 
#> $hatch_green_npoints
#> [1] 200
#> 
#> $hatch_green_nlines
#> [1] 100
#> 
#> $hatch_green_type
#> [1] "lines"
#> 
#> $hatch_green_size
#> [1] 1
#> 
#> $hatch_green_alpha
#> [1] 0.1
#> 
#> $palette_building
#> [1] "#7e6e55" "#9c8c6e" "#6a5944"
#> 
#> $railway
#> [1] "#a35e48"
#> 
#> $green
#> [1] "#c5d1a5"
#> 
#> $water
#> [1] "#9dbcd4"
#> 
#> $background
#> [1] "#f3efe2"
#> 
#> $street
#> [1] "#b0a18f"
#> 
#> $beach
#> [1] "#e2d1b3"
#> 
#> $parking
#> [1] "#a9a18c"
#> 
#> attr(,"class")
#> [1] "cartographr_palette"

# Custom palette creation using a named list for a simple black and white palette
custom_palette <- list(
  palette_building = c("#000000", "#FFFFFF", "#CCCCCC"),
  water = "#000000",
  green = "#FFFFFF",
  beach = "#000000",
  parking = "#FFFFFF",
  street = "#000000",
  background = "#CCCCCC",
  railway = "#000000",
  hatch_water = TRUE,
  linewidth_buildings = 0.05,
  linewidth_motorway = 6,
  linewidth_primary = 4,
  linewidth_secondary = 4,
  linewidth_tertiary=3,
  linewidth_unclassified = 3,
  linewidth_residential = 3,
  linewidth_pedestrian = 1,
  linewidth_service = 1,
  linewidth_living_street = 1,
  size_hatch = 1,
  alpha_hatch = 0.1,
  size_streetlamp = 0.2
)

get_palette(custom_palette)
#> $name
#> [1] "custom"
#> 
#> $border_color
#> [1] "#121212"
#> 
#> $border_width
#> [1] 0.001
#> 
#> $linewidth_buildings
#> [1] 0.05
#> 
#> $linewidth_motorway
#> [1] 6
#> 
#> $linewidth_trunk
#> [1] 6
#> 
#> $linewidth_primary
#> [1] 4
#> 
#> $linewidth_secondary
#> [1] 4
#> 
#> $linewidth_tertiary
#> [1] 3
#> 
#> $linewidth_unclassified
#> [1] 3
#> 
#> $linewidth_residential
#> [1] 3
#> 
#> $linewidth_pedestrian
#> [1] 1
#> 
#> $linewidth_service
#> [1] 1
#> 
#> $linewidth_living_street
#> [1] 1
#> 
#> $size_streetlamp
#> [1] 0.2
#> 
#> $hatch_water
#> [1] TRUE
#> 
#> $hatch_water_npoints
#> [1] 200
#> 
#> $hatch_water_nlines
#> [1] 100
#> 
#> $hatch_water_type
#> [1] "points"
#> 
#> $hatch_water_size
#> [1] 1
#> 
#> $hatch_water_alpha
#> [1] 0.1
#> 
#> $hatch_buildings
#> [1] FALSE
#> 
#> $hatch_buildings_npoints
#> [1] 200
#> 
#> $hatch_buildings_nlines
#> [1] 100
#> 
#> $hatch_buildings_type
#> [1] "points"
#> 
#> $hatch_buildings_size
#> [1] 1
#> 
#> $hatch_buildings_alpha
#> [1] 0.1
#> 
#> $hatch_green
#> [1] FALSE
#> 
#> $hatch_green_npoints
#> [1] 200
#> 
#> $hatch_green_nlines
#> [1] 100
#> 
#> $hatch_green_type
#> [1] "lines"
#> 
#> $hatch_green_size
#> [1] 1
#> 
#> $hatch_green_alpha
#> [1] 0.1
#> 
#> $palette_building
#> [1] "#000000" "#FFFFFF" "#CCCCCC"
#> 
#> $water
#> [1] "#000000"
#> 
#> $green
#> [1] "#FFFFFF"
#> 
#> $beach
#> [1] "#000000"
#> 
#> $parking
#> [1] "#FFFFFF"
#> 
#> $street
#> [1] "#000000"
#> 
#> $background
#> [1] "#CCCCCC"
#> 
#> $railway
#> [1] "#000000"
#> 
#> $size_hatch
#> [1] 1
#> 
#> $alpha_hatch
#> [1] 0.1
#> 
#> attr(,"class")
#> [1] "cartographr_palette"
```
