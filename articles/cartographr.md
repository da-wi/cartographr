# Introduction to cartographr

In this vignette, you will learn how to add new information to a map.
Here, we retrieve a map of Manhattan, NYC.

``` r

library(cartographr)
library(dplyr)
library(ggplot2)
library(sf)
```

First, we load the locations of crimes in Manhattan.

``` r

data("crime") 
data("soho_boundary")
```

## Convert longitude / latitude into a `sf`

To add the crime locations in the dataset to our map, we harness the
`sf` (simple features) package, which includes a lot of useful tools for
working with geo data. To this end, we convert the dataset into a `sf`
object using latitude and longitude.

``` r

crime |> head()
```

This can be achieved with the function
[`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html)
using the coordinates from the dataset.

## Setup the map parameters

We have to provide `longitude`, `latitude` and `x_distance` (i.e., the
width of the map in meters). Furthermore, we define the extend of the
OSM data in meters in
[`get_osmdata()`](https://da-wi.github.io/cartographr/reference/get_osmdata.md).
`y_distance` is calculated automatically using the output size aspect
ratio.

``` r

set_output_size("A4", orientation = "portrait")
osm <- get_osmdata(sf = soho_boundary)
```

## Plot the map

[`plot_map()`](https://da-wi.github.io/cartographr/reference/plot_map.md)
generates a `ggplot2` object using the color theme set as parameter.
That means that we can easily adjust the plot using `ggplot2` commands
and also add new information to the map.

``` r

p <- osm |> 
  crop(soho_boundary) |> 
  plot_map(palette = "alphabet") +
  theme_infomap_barlow() + 
  
  # Add geom with crimes
  geom_point(data = crime,
             aes(x = longitude, y = latitude, shape = type), color="#A72424", size=2) + 
  
  # Set labels
  labs(title = "CRIME IN SOHO",
       shape = "TYPE")
```

Now we can plot the map by simply calling its print method implicity:

``` r

p
```

![](https://github.com/da-wi/cartographr/raw/develop/png/plot%20the%20map-1.png)

## Save map

[`save_map()`](https://da-wi.github.io/cartographr/reference/save_map.md)
can be used to store the print-ready plot object to the disk as a drawn
object in `pdf` format.

``` r

save_map(plot = p, filename = "ny_crime.pdf")
```
