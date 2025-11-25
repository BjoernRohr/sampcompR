# Compare data frames and Plot Differences

Returns data or a plot showing the difference of two or more data frames
The differences are calculated on the base of differing metrics, chosen
in the funct argument. All used data frames must contain at least one
column named equal in all data frames, that has equal values.

## Usage

``` r
uni_compare(
  dfs,
  benchmarks,
  variables = NULL,
  nboots = 2000,
  n_bench = NULL,
  boot_all = FALSE,
  funct = "rel_mean",
  data = TRUE,
  type = "comparison",
  legendlabels = NULL,
  legendtitle = NULL,
  colors = NULL,
  shapes = NULL,
  summetric = "rmse2",
  label_x = NULL,
  label_y = NULL,
  plot_title = NULL,
  varlabels = NULL,
  name_dfs = NULL,
  name_benchmarks = NULL,
  summet_size = 4,
  silence = TRUE,
  conf_level = 0.95,
  conf_adjustment = NULL,
  percentile_ci = TRUE,
  weight = NULL,
  id = NULL,
  strata = NULL,
  weight_bench = NULL,
  id_bench = NULL,
  strata_bench = NULL,
  adjustment_weighting = "raking",
  adjustment_vars = NULL,
  raking_targets = NULL,
  post_targets = NULL,
  ndigits = 3,
  parallel = FALSE
)
```

## Arguments

- dfs:

  A character vector containing the names of data frames to compare
  against the benchmarks.

- benchmarks:

  A character vector containing the names of benchmarks to compare the
  data frames against. The vector must either be the same length as
  `dfs`, or length 1. If it has length 1 every df will be compared
  against the same benchmark. Benchmarks can either be the name of data
  frames, the name of a list of tables, or a named vector of means. The
  tables in the list need to be named as the respective variables in the
  data frame of comparison. When they are a named vector of means, the
  means need to be named as the respective variables in the dfs.

- variables:

  A character vector containing the names of the variables for the
  comparison. If NULL, all variables named similarly in both the `dfs`
  and the benchmarks will be compared. Variables missing in one of the
  data frames or the benchmarks will be neglected for this comparison.

- nboots:

  The number of bootstraps used to calculate standard errors. Must
  either be \>2 or 0. If \>2 bootstrapping is used to calculate standard
  errors with `nboots` iterations. If 0, SE is calculated analytically.
  We do not recommend using `nboots` =0 because this method is not yet
  suitable for every `funct` used and every method. Depending on the
  size of the data and the number of bootstraps, `uni_compare` can take
  a while.

- n_bench:

  A list of vectors containing the number of cases for every variable in
  the benchmark. This is only needed, if the benchmark is given as a
  vector. The list should be as long as the number of dataframes

- boot_all:

  If TURE, both, dfs and benchmarks will be bootstrapped. Otherwise the
  benchmark estimate is assumed to be constant.

- funct:

  A character string, indicating the function to calculate the
  difference between the data frames.

  Predefined functions are:

  - `"d_mean"`, `"ad_mean"` A function to calculate the (absolute)
    difference in mean of the variables in `dfs` and benchmarks with the
    same name. Only applicable for metric variables.

  - `"d_prop"`, `"ad_prop"` A function to calculate the (absolute)
    difference in proportions of the variables in `dfs` and benchmarks
    with the same name. Only applicable for dummy variables.

  - `"rel_mean"`, `"abs_rel_mean"` A function to calculate the
    (absolute) relative difference in mean of the variables in `dfs` and
    benchmarks with the same name. \#' For more information on the
    formula for difference and analytic variance, see Felderer et al.
    (2019). Only applicable for metric variables.

  - `"rel_prop"`, `"abs_rel_prop"` A function to calculate the
    (absolute) relative difference in proportions of the variables in
    `dfs` and benchmarks with the same name. It is calculated similar to
    the relative difference in mean (see Felderer et al., 2019), however
    the default label for the plot is different. Only applicable for
    dummy variables.

- data:

  If TRUE, a uni_compare_object is returned, containing results of the
  comparison.

- type:

  Define the type of comparison. Can either be `"comparison"` or
  `"nonresponse"`.

- legendlabels:

  A character string or vector of strings containing a label for the
  legend.

- legendtitle:

  A character string containing the title of the legend.

- colors:

  A vector of colors, that is used in the plot for the different
  comparisons.

- shapes:

  A vector of shapes applicable in
  [`ggplot2::ggplot2()`](https://ggplot2.tidyverse.org/reference/ggplot2-package.html),
  that is used in the plot for the different comparisons.

- summetric:

  If `"avg1"`, `"mse1"`, `"rmse1"`, or `"R"` the respective measure is
  calculated for the biases of each survey. The values `"mse1"` and
  `"rmse1"` lead to similar results as in `"mse2"` and `"rmse2"`, with
  slightly different visualization in the plot. If `summetric = NULL`,
  no summetric will be displayed in the Plot. When `"R"` is chosen, also
  `response_identificator` is needed.

- label_x, label_y:

  A character string or vector of character strings containing a label
  for the x-axis and y-axis.

- plot_title:

  A character string containing the title of the plot.

- varlabels:

  A character string or vector of character strings containing the new
  names of variables, also used in plot.

- name_dfs, name_benchmarks:

  A character string or vector of character strings containing the new
  names of the `dfs` and `benchmarks`, that is also used in plot.

- summet_size:

  A number to determine the size of the displayed `summetric` in the
  plot.

- silence:

  If `silence = FALSE` a warning will be displayed, if variables a re
  excluded from either the data frame or benchmark, for not existing in
  both.

- conf_level:

  A numeric value between zero and one to determine the confidence level
  of the confidence interval.

- conf_adjustment:

  If `conf_adjustment = TRUE` the confidence level of the confidence
  interval will be adjusted with a Bonferroni adjustment, to account for
  the problem of multiple comparisons.

- percentile_ci:

  If TURE, cofidence intervals will be calculated using the percentile
  method. If False, they will be calculated using the normal method.

- weight, weight_bench:

  A character vector determining variables to weight the `dfs` or
  `benchmarks`. They have to be part of the respective data frame. If
  only one character is provided, the same variable is used to weigh
  every `df` or `benchmark`. If a weight variable is provided also an
  `id` variable is needed.For weighting, the `survey` package is used.

- id, id_bench:

  A character vector determining `id` variables used to weigh the `dfs`
  or `benchmarks` with the help of the `survey` package. They have to be
  part of the respective data frame. If only one character is provided,
  the same variable is used to weigh every `df` or `benchmark`.

- strata, strata_bench:

  A character vector determining strata variables used to weigh the
  `dfs` or `benchmarks` with the help of the `survey` package.They have
  to be part of the respective data frame. If only one character is
  provided, the same variable is used to weight every `df` or
  `benchmark`.

- adjustment_weighting:

  A character vector indicating if adjustment weighting should be used.
  It can either be `"raking"` or `"post_start"`.

- adjustment_vars:

  Variables used to adjust the survey when using raking or post
  stratification.

- raking_targets:

  A list of raking targets that can be given to the rake function of
  [`rake`](https://rdrr.io/pkg/survey/man/rake.html), to rake the `dfs`.

- post_targets:

  A list of post-stratification targets that can be given to the
  [`postStratify`](https://rdrr.io/pkg/survey/man/postStratify.html)
  function, to post-stratify the `dfs`.

- ndigits:

  The number of digits to round the numbers in the plot.

- parallel:

  Can be either `FALSE` or a number of cores that should be used in the
  function. If it is `FALSE`, only one core will be used and otherwise
  the given number of cores will be used.

## Value

A plot based on
[`ggplot2::ggplot2()`](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
(or data frame if data==TRUE) which shows the difference between two or
more data frames on predetermined variables, named identical in both
data frames.

## References

Felderer, B., Kirchner, A., & Kreuter, FALSE. (2019). The Effect of
Survey Mode on Data Quality: Disentangling Nonresponse and Measurement
Error Bias. Journal of Official Statistics, 35(1), 93–115.
https://doi.org/10.2478/jos-2019-0005

## Examples

``` r
## Get Data for comparison

data("card")

north<-card[card$south==0,]
white<-card[card$black==0,]

## use the function to plot the data 
univar_comp<-sampcompR::uni_compare(dfs = c("north","white"),
                                    benchmarks = c("card","card"),
                                    variables= c("age","educ","fatheduc","motheduc","wage","IQ"),
                                    funct = "abs_rel_mean",
                                    nboots=200,
                                    summetric="rmse2",
                                    data=FALSE)
#> Error in get(dfs[i]): object 'north' not found

 univar_comp
#> Error: object 'univar_comp' not found
 
```
