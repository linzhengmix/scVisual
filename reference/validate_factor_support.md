# Validate and warn about factor status

Check if a column is a factor and provide helpful warning if not,
suggesting factor conversion for custom ordering.

## Usage

``` r
validate_factor_support(data, colname, context = "plotting")
```

## Arguments

- data:

  Data frame containing the column

- colname:

  Name of the column to check

- context:

  Context for the warning message
