# Apply infomap theme with specified font

This function applies a custom theme for information maps, allowing the
user to specify a font from a predefined list. It sets various ggplot2
theme elements such as the title, subtitle, caption, and legend to use
the specified font and adjusts their appearance based on a scale factor.

## Usage

``` r
theme_infomap(font = "Poppins")

theme_infomap_anton()

theme_infomap_poppins()

theme_infomap_cinzel()

theme_infomap_barlow()
```

## Arguments

- font:

  A character string specifying the font to use for the theme elements.
  The default is "Poppins". Only "Poppins", "Anton", "Cinzel", and
  "Barlow" are valid options.

## Value

A 'ggplot2' theme object with the information map theme settings
applied.

## Details

`theme_infomap_poppins()`, `theme_infomap_anton()` are aliases to
`theme_infomap("Poppins")`, etc.

## Examples

``` r
data("osm")
my_map <- osm |> plot_map() +
 theme_infomap_poppins()
```
