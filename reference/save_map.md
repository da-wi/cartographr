# Save a map to a file

This function saves a ggplot object to a file using the specified
filename. It checks for the orientation setting and warns if the scale
factor has changed after the plot was created.

## Usage

``` r
save_map(plot, filename, device = "pdf")
```

## Arguments

- plot:

  A ggplot object representing the map to be saved.

- filename:

  A character string specifying the path and name of the file to save
  the plot to.

- device:

  The output device defaulting to pdf

## Value

The function saves the plot to a file and does not return anything.

## Examples

``` r
# \donttest{
data("osm")
my_map <- osm |> plot_map()
filename <- tempfile(fileext =  ".pdf")
save_map(my_map, filename)
unlink(filename)
# }
```
