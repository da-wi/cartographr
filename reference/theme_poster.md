# Create a poster theme

This function generates a 'ggplot2' theme that resembles a poster style.
It is designed to be used with 'ggplot2' plots to provide a clean and
bold aesthetic suitable for poster visuals.

## Usage

``` r
theme_poster(font = "Poppins")

theme_poster_poppins()

theme_poster_anton()

theme_poster_cinzel()

theme_poster_barlow()
```

## Arguments

- font:

  The font family to be used for text elements in the plot. The default
  font is set to "Poppins".

## Value

A 'ggplot2' theme object that can be added to 'ggplot2' plotting calls.

## Details

`theme_poster_poppins()`, `theme_poster_anton()` are aliases to
`theme_poster("Poppins")`, etc.

## Examples

``` r
data("osm")
my_map <- osm |> plot_map() +
 theme_poster()
```
