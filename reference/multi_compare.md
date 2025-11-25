# Compares data frames using different regression methods.

`multi_compare` compares data frames using regression models based on
differing methods. All [`glm`](https://rdrr.io/r/stats/glm.html) Models
can be compared.

## Usage

``` r
multi_compare(
  df,
  benchmark,
  independent = NULL,
  dependent = NULL,
  formula_list = NULL,
  family = "ols",
  rm_na = "pairwise",
  out_output_list = TRUE,
  out_df = FALSE,
  out_models = FALSE,
  print_p = FALSE,
  print_se = FALSE,
  weight = NULL,
  id = NULL,
  strata = NULL,
  nest = FALSE,
  weight_bench = NULL,
  id_bench = NULL,
  strata_bench = NULL,
  nest_bench = FALSE,
  robust_se = FALSE,
  p_adjust = NULL,
  names_df_benchmark = NULL,
  silence_summary = FALSE,
  nboots = 0,
  boot_all = FALSE,
  parallel = FALSE,
  adjustment_vars = NULL,
  raking_targets = NULL,
  post_targets = NULL,
  percentile_ci = TRUE
)
```

## Arguments

- df, benchmark:

  A data frame containing the set of respondents or benchmark set of
  respondents to compare, or a character string containing the name of
  the set of respondents or benchmark set of respondents. All
  independent and dependent variables must be inside both data frames.

- independent:

  A list of strings containing the independent variables (x) for
  comparison. Every independent variable will be used in every model to
  estimate the dependent variable (y). When a `formula_list` is
  provided, `independent` will be ignored.

- dependent:

  A list of strings containing the dependent variables (y) for
  comparison. One model will be computed for every dependent
  variable (y) provided. When a `formula_list` is provided, `dependent`
  will be ignored.

- formula_list:

  A list of formulas to use in the regression models. If given,
  `dependent` and `independent` parameters will be ignored.

- family:

  A family input, that can be given to
  [`glm`](https://rdrr.io/r/stats/glm.html) or
  [`svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html). Additionally,
  if "ols" is given, `gaussian(link = "identity")`, and if "logit" is
  given, `binomial(link = "logit")` is used.

- rm_na:

  A character to determine how to handle missing values. For this two
  options are supported. If `rm_na = "pairwise"` NAs will be removed
  separately for every model. Only cases containing NA on one of the
  variables used in the respective model will be removed (all
  independent variables but only the respective dependent variable). If
  `rm_na = "listwise"` all cases containing NA on one of the dependent
  or independent variables are removed.

- out_output_list:

  A logical value. If `out_output_list = TRUE`, a list will be returned,
  containing the separate interaction models calculated with the
  [`glm`](https://rdrr.io/r/stats/glm.html) function or
  [`svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html) in case of
  weighting, as well as a summary object for every model. Standard
  errors and p-values of these models are always calculated without
  robustness methods.

- out_df:

  If `TRUE`, the used data frames will also be part of the output list.

- out_models:

  If True, GLM model objects will be part of the returned object.

- print_p:

  If `TRUE`, in addition to the difference in Average Discrete Change
  (ADC), p-values will be printed.

- print_se:

  If `TRUE`, additionally standard errors will be printed.

- weight, weight_bench:

  A character vector containing the name of the weight variable in the
  respective data frame. If provided the data frame will be weighted
  using the [`svydesign`](https://rdrr.io/pkg/survey/man/svydesign.html)
  function. Also `id` must be provided.

- id, id_bench:

  A character vector containing the name of the id variable in the
  respectiv data frame. Only needed for weighting.

- strata, strata_bench:

  A character vector containing the name of the strata variable in the
  respective data frame. It is used in the
  [`svydesign`](https://rdrr.io/pkg/survey/man/svydesign.html) function
  for weighting.

- nest, nest_bench:

  A logical vector that is used in the
  [`svydesign`](https://rdrr.io/pkg/survey/man/svydesign.html) function
  for the respective data frame.

- robust_se:

  A logical value If `TRUE` instead of normal standard errors,
  heteroscedasticity-consistent standard errors will be used in the
  analysis to calculate them the
  [`vcovHC`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html)
  and [`coeftest`](https://rdrr.io/pkg/lmtest/man/coeftest.html)
  packages are used.

- p_adjust:

  A logical input or character string indicating an adjustment method
  usable in the `method` parameter of
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html). If set to TRUE
  the Bonferroni adjusted p-values are used in inference.

- names_df_benchmark:

  A vector containing first the name of `df` and benchmark.

- silence_summary:

  A logical value, to indicate if the printed summary should not be
  printed instead.

- nboots:

  A numeric value indicating the number of bootstrap replications. If
  nboots = 0 no bootstrapping will be performed. Else `nboots` must be
  \>2. Note, that bootstrapping can be very computationaly heavy and can
  therefore take a while.

- boot_all:

  If TURE, both, dfs and benchmarks will be bootstrapped. Otherwise the
  benchmark estimate is assumed to be constant.

- parallel:

  If `TRUE`, all detected cores will be used in bootstrapping.

- adjustment_vars:

  Variables used to adjust the survey when using raking or
  post-stratification.

- raking_targets:

  A List of raking targets that can be given to the rake function of
  [`rake`](https://rdrr.io/pkg/survey/man/rake.html), to rake the `df`.

- post_targets:

  A List of post_stratification targets that can be given to the rake
  function of
  [`postStratify`](https://rdrr.io/pkg/survey/man/postStratify.html), to
  post_stratificatify the `df`.

- percentile_ci:

  If TURE, cofidence intervals will be calculated using the percentile
  method. If False, they will be calculated using the normal method.

## Value

A table is printed showing the difference between the set of respondents
for each model, as well as an indicator, if they differ significantly
from each other. It is generated using the chosen `method`.
If`out_output_list` = TRUE, also a list with additional information will
be returned that can be used in some additional packages of this
function to reprint the summary or to visualize the results.

## Examples

``` r
#Example 1
## Make a comparison specifiying dependent and independent variables.

## Get Data for comparison

data("card")

north <- card[card$south==0,]


## use the function to plot the data 
multi_data1<-sampcompR::multi_compare(df = north, 
                                     bench = card,
                                     independent = c("age","fatheduc","motheduc","IQ"),
                                     dependent = c("educ","wage"),
                                     family="ols") 
#> 
#> Difference in coeficients between sets of respondents 
#>  
#>          educ         wage        
#> age      -2.43e-02    -8.55e-01   
#> fatheduc -2.37e-02    -2.93e-01   
#> motheduc 1.23e-02     2.35e+00    
#> IQ       -7.25e-03    4.80e-01    
#> 
#> Overall difference between north & card: 0% of coeficients are significant different
#> (*p<0.05 ; **p<0.005 ; ***p<0.001;  for t-test robust standard errors are used) 
#>  
                        
plot_multi_compare("multi_data1")
#> Error in get(multi_compare_objects[i]): object 'multi_data1' not found

#Example 2
## Make a comparison with a formula_list
data("card")

north <- card[card$south==0,]

form_list<-list(formula(educ~age+fatheduc+motheduc+IQ),
                formula(wage~age+fatheduc+motheduc+IQ))


multi_data2 <- sampcompR::multi_compare(df = north, 
                                        bench = card,
                                        formula_list = form_list,
                                        family="ols")
#> 
#> Difference in coeficients between sets of respondents 
#>  
#>          educ         wage        
#> age      -2.43e-02    -8.55e-01   
#> fatheduc -2.37e-02    -2.93e-01   
#> motheduc 1.23e-02     2.35e+00    
#> IQ       -7.25e-03    4.80e-01    
#> 
#> Overall difference between north & card: 0% of coeficients are significant different
#> (*p<0.05 ; **p<0.005 ; ***p<0.001;  for t-test robust standard errors are used) 
#>  


plot_multi_compare("multi_data2")
#> Error in get(multi_compare_objects[i]): object 'multi_data2' not found
```
