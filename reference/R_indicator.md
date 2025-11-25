# Calculate the R-Indicator

Calculates the R-Indicator of the (weighted) data frame.

## Usage

``` r
R_indicator(
  dfs,
  response_identificators,
  variables,
  id = NULL,
  weight = NULL,
  strata = NULL,
  get_r2 = FALSE
)
```

## Arguments

- dfs:

  A character vector containing the names of data frames to calculate
  the R indicator.

- response_identificators:

  A character vector, naming response identificators for every df.
  Response identificators should indicate if respondents are part of the
  set of respondents `(respondents = 1)` or not part of the set of
  respondents. `(non-respondents = 0)`. If only one character is
  provided, the same variable is used in every df.

- variables:

  A character vector with the names of variables that should be used in
  the model to calculate the R indicator.

- id:

  A character vector that determines id variables that are used to
  weight the dfs with the help of the survey package. They have to be
  part of the respective data frame. If only one character is provided,
  the same variable is used to weight every df.

- weight:

  A character vector that determines variables to weight the dfs. They
  have to be part of the respective data frame. If only one character is
  provided, the same variable used to weight every df. If a weight
  variable is provided also an id variable is needed. For weighting, the
  survey package is used.

- strata:

  A character vector that determines strata variables that are used to
  weight the dfs with the help of the survey package. They have to be
  part of the respective data frame. If only one character is provided,
  the same variable is used to weight every df.

- get_r2:

  If true, Pseudo R-squared of the propensity model will be returned,
  based on the method of McFadden.

## Value

A list containing the R-indicator, and its standard error for every data
frame.

## Note

The calculated R-indicator is based on Shlomo et al., (2012).

## References

- Shlomo, N., Skinner, C., & Schouten, B. (2012). Estimation of an
  indicator of the representativeness of survey response. Journal of
  Statistical Planning and Inference, 142(1), 201–211.
  https://doi.org/10.1016/j.jspi.2011.07.008

## Examples

``` r

data("card")

# For the purpose of this example, we assume that only respondents living in 
# the south or only white respondents have participated in the survey.

sampcompR::R_indicator(dfs=c("card","card"),
                       response_identificators = c("south","black"),
                       variables = c("age","educ","fatheduc","motheduc","wage","IQ"),
                       weight = c("weight","weight"))
#> Warning: non-integer #successes in a binomial glm!
#> Warning: non-integer #successes in a binomial glm!
#> $card
#> R-Indicator          SE 
#>  0.86107378  0.02333811 
#> 
#> $card
#> R-Indicator          SE 
#>  0.85336485  0.02227787 
#> 
```
