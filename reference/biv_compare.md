# Compare Multiple Data Frames on a Bivariate Level

Compare multiple data frames on a bivariate level and plot them
together.

## Usage

``` r
biv_compare(
  dfs,
  benchmarks,
  variables = NULL,
  corrtype = "r",
  data = TRUE,
  id = NULL,
  weight = NULL,
  strata = NULL,
  id_bench = NULL,
  weight_bench = NULL,
  strata_bench = NULL,
  p_value = NULL,
  p_adjust = NULL,
  varlabels = NULL,
  plot_title = NULL,
  plots_label = NULL,
  diff_perc = TRUE,
  diff_perc_size = 4.5,
  perc_diff_transparance = 0,
  note = FALSE,
  order = NULL,
  breaks = NULL,
  colors = NULL,
  mar = c(0, 0, 0, 0),
  grid = "white",
  gradient = FALSE,
  sum_weights = NULL,
  missings_x = TRUE,
  remove_nas = "pairwise",
  ncol_facet = 3,
  nboots = 0,
  boot_all = FALSE,
  parallel = FALSE,
  adjustment_weighting = "raking",
  adjustment_vars = NULL,
  raking_targets = NULL,
  post_targets = NULL,
  percentile_ci = TRUE
)
```

## Arguments

- dfs:

  A character vector containing the names of data frames to compare
  against the `benchmarks`.

- benchmarks:

  A character vector containing the names of benchmarks to compare the
  `dfs` against, or the names of a list. If it is a list, it has to be
  of the form, as the output of
  [rcorr](https://rdrr.io/pkg/Hmisc/man/rcorr.html), with a Pearson's r
  matrix in the first position, a n-matrix (matrix of n for every
  correlation) in the second position and a p-matrix in the third
  position. The vector must either be the same length as `dfs`, or
  length 1. If it has length one every survey will be compared against
  the same benchmark.

- variables:

  A character vector that containes the names of the variables for the
  comparison. If it is `NULL`, all variables that are named similarly in
  both the `dfs` and the benchmarks will be compared. Variables missing
  in one of the `dfs` or the `benchmarks` will be neglected for this
  comparison.

- corrtype:

  A character string, indicating the type of the bivariate correlation.
  It can either be "r" for Pearson's r or "rho" for Spearman's "rho". At
  the moment, rho is only applicable to unweighted data.

- data:

  If `TRUE`, a biv_compare object is returned, containing the results of
  the comparison.

- strata, strata_bench:

  A character vector that determines strata variables that are used to
  weigh the `dfs` or `benchmarks` with the help of the `survey` package.
  It has to be part of the respective data frame. If fewer characters
  strings are provided, than in `dfs`, the first input is used to weigh
  every df or benchmark, where no input is provided.

- id_bench, id:

  A character vector determining id variables used to weigh the `dfs` or
  `benchmarks` with the help of the `survey` package. They have to be
  part of the respective data frame. If less characters strings are
  provided, than in `dfs`, the first input is used to weigh every `df`
  or `benchmark`, where no input is provided.

- weight_bench, weight:

  A character vector that determines variables to weigh the `dfs` of
  `benchmarks`. They have to be part of the respective data frame. If
  fewer characters strings are provided, than in `dfs`, the first input
  is used to weigh every df or benchmark, where no input is provided. If
  a weight variable is provided also an id variable is needed. For
  weighting, the `survey` package is used.

- p_value:

  A number between zero and one to determine the maximum significance
  niveau.

- p_adjust:

  Can be either `TRUE` or a character string indicating an adjustment
  method. If `p_adjust = TRUE` the p_values will be adjusted with the
  Bonferroni adjustment method, by default, to account for the problem
  of multiple comparisons. All adjustment methods available in
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html) can be used here,
  with the same character strings.

- varlabels:

  A character string or vector of character strings containing the new
  names of variables that is used in the plot.

- plot_title:

  A character string containing the title of the plot.

- plots_label:

  A character string or vector of character strings containing the new
  names of the data frames that are used in the plot.

- diff_perc:

  If `TRUE` a percental difference between surveys and benchmarks is
  displayed in the plot.

- diff_perc_size:

  A number to determine the size of the displayed percental difference
  between surveys in the plot.

- perc_diff_transparance:

  A number to determine the transparency of the displayed percental
  difference between surveys in the plot.

- note:

  If `note = TRUE`, a note will be displayed to describe the plot.

- order:

  A character vector to determine in which order the variables should be
  displayed in the plot.

- breaks:

  A vector to label the color scheme in the legend.

- colors:

  A vector to determine the colors in the plot.

- mar:

  A vector that determines the margins of the plot.

- grid:

  A color string, that determines the color of the lines between the
  tiles of the heatmap.

- gradient:

  If `gradient = TRUE`, colors in the heatmap will be more or less
  transparent, depending on the difference in Pearson's r of the data
  frames of comparison.

- sum_weights:

  A vector containing information for every variable to weigh them in
  the displayed percental-difference calculation. It can be used if some
  variables are over- or underrepresented in the analysis.

- missings_x:

  If `TRUE`, missing pairs in the plot will be marked with an X.

- remove_nas:

  A character string, that indicates how missing values should be
  removed, can either be `"all"`, to remove all cases that contain NA in
  any of the variables, or `"pairwise"`, to remove NAs separately for
  every variable pair when calculating Pearson's r.

- ncol_facet:

  The number of columns used in faced_wrap() for the plots.

- nboots:

  A numeric value indicating the number of bootstrap replications. If
  `nboots = 0` no bootstrapping will be performed. Else `nboots` must be
  \>2. Note, that bootstrapping can be very computationally heavy and
  can therefore take a while.

- boot_all:

  If TURE, both, dfs and benchmarks will be bootstrapped. Otherwise the
  benchmark estimate is assumed to be constant.

- parallel:

  Can be either `FALSE` or a number of cores that should be used in the
  function. If it is `FALSE`, only one core will be used and otherwise
  the given number of cores will be used.

- adjustment_weighting:

  A character vector indicating if adjustment weighting should be used.
  It can either be `"raking"` or `"post_start"`.

- adjustment_vars:

  Variables used to adjust the survey when using raking or
  post-stratification.

- raking_targets:

  A list of raking targets that can be given to the rake function of
  [`rake`](https://rdrr.io/pkg/survey/man/rake.html), to rake the `dfs`.

- post_targets:

  A list of post_stratification targets that can be given to the
  [`postStratify`](https://rdrr.io/pkg/survey/man/postStratify.html)
  function, to post-stratify the `dfs`.

- percentile_ci:

  If TURE, cofidence intervals will be calculated using the percentile
  method. If False, they will be calculated using the normal method.

## Value

A object generated with the help of
[`ggplot2::ggplot2()`](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
visualizes the differences between the data frames and benchmarks. If
`data = TRUE` instead of the plot a list will be returned containing
information of the analyses. This `biv_compare` object can be used in
`plot_biv_compare` to build a plot, or in `biv_compare_table`, to get a
table.

## Details

The plot shows a heatmap of a correlation matrix, where the colors are
determined by the similarity of the Pearson's r values in both sets of
respondents. Leaving default breaks and colors,

- `Same` (green) indicates, that the Pearson's r correlation is not
  significant \> 0 in the related data frame or benchmark or the
  Pearson's r correlations are not significantly different, between data
  frame and benchmark.

- `Small Diff` (yellow) indicates that the Pearson's r correlation is
  significant \> 0 in the related data frame or benchmark and the
  Pearson's r correlations are significantly different, between data
  frame and benchmark.

- `Large Diff` (red) indicates, that the same conditions of yellow are
  fulfilled, and the correlations are either in opposite directions,or
  one is double the size of the other.

## Examples

``` r
## Get Data for comparison

data("card")

north <- card[card$south==0,]
white <- card[card$black==0,]

## use the function to plot the data 
bivar_comp<-sampcompR::biv_compare(dfs = c("north","white"),
                                   benchmarks = c("card","card"),
                                   variables= c("age","educ","fatheduc","motheduc","wage","IQ"),
                                   data=FALSE)
#> Error in get(dfs[i]): object 'north' not found
bivar_comp
#> Error: object 'bivar_comp' not found

```
