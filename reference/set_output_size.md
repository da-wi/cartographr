# Set output size for maps

This function sets the output size for cartographic displays. It allows
the user to specify a standard paper size or custom dimensions. If no
size is specified, it returns the current output size.

## Usage

``` r
set_output_size(size = NULL, orientation = "portrait")
```

## Arguments

- size:

  A character string specifying the standard paper size or a numeric
  vector with custom dimensions (width, height). The standard sizes can
  be one of "A0", "A1", "A2", "A3", "A4", "A5", "A6", "small_poster",
  "medium_poster", or "large_poster". If size is NULL, the current
  output size is returned.

- orientation:

  The orientation of the output

## Value

If size is NULL, returns the current output size as a numeric vector. If
a size is specified, the function sets the output size..

## Examples

``` r
set_output_size("A3") # Sets the output size to A3 dimensions
set_output_size(c(300, 200)) # Sets a custom output size
```
