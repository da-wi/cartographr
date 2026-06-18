# Plot a map with custom palette

This function takes an 'osmdata' (osm) object and a palette name,
preprocesses the map data if not already done, and plots the map using
'ggplot2' with the specified color palette.

## Usage

``` r
plot_map(...)
```

## Arguments

- ...:

  Variable argument list:

  - `osm`: A list retrieved from osmdata containing map data.

  - `palette`: A character string specifying the name of the palette to
    use. The default is "imhof". Additional arguments are passed on to
    the preprocessing and plotting functions.

## Value

A `ggplot` object representing the map with the chosen palette.

## Examples

``` r
data("osm")
my_map <- osm |> plot_map(palette = 'gray')
```
