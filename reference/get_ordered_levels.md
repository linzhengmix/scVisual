# Get ordered levels from a vector

Get levels from factor if available, otherwise unique values. This
function maintains the custom order of factors and provides consistent
behavior across different data types.

## Usage

``` r
get_ordered_levels(x)
```

## Arguments

- x:

  A vector to process

## Value

Ordered levels maintaining factor order if factor, otherwise unique
values
