# Set or get attribution setting

This function sets a new value for the attribution setting in the
cartographr environment or retrieves the current setting if no argument
is provided.

## Usage

``` r
set_attribution(attribution = NULL)
```

## Arguments

- attribution:

  A logical value to set the acknowledgments setting. If `NULL`, the
  current setting is returned. The default is `NULL`.

## Value

If `attribution` is `NULL`, returns the current acknowledgments setting.
If `attribution` is a logical value, the function will set the
acknowledgments setting to that value and return invisibly.

## Examples

``` r
# To get the current acknowledgments setting
set_attribution()
#> [1] TRUE

# To set the acknowledgments setting to TRUE
set_attribution(TRUE)
```
