# Get a Descriptive Table for Every Data Frame

Get a Descriptive Table for every Data Frame, to easy document your Data

## Usage

``` r
descriptive_table(
  dfs,
  variables,
  varlabels = NULL,
  weight = NULL,
  strata = NULL,
  id = NULL,
  value = "mean",
  digits = 3
)
```

## Arguments

- dfs:

  A character vector, containing the names of the data frames.

- variables:

  A character vector containing the variables in the data frame that
  should be described.

- varlabels:

  A character vector containing the Labels for every variable in
  variables.

- weight:

  A character vector, containing either the name of a weight in the
  respective data frame, or NA, if no weighting should be performed for
  this data frame.

- strata:

  A character vector, containing either the name of a strata in the
  respective data frame, or NA, if no strata should be used when
  weighting this data frame.

- id:

  A character vector, containing either the name of a id in the
  respective data frame, or NA, if every row is unique for this data
  frame.

- value:

  A character vector indicating what descriptive value should be
  displayed for the data frame. It can either be "mean", "percent",
  "total", or "total_percent".

- digits:

  A numeric value indicating the number of digits that the Descriptive
  table should be rounded to.

## Value

Returns a matrix of Descriptive information. Output depends on value.
