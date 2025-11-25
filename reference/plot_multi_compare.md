# Plot Multiple multi_compare_objects

`plot_multi_compare` plots multipe `multi_compare_objects` together.

## Usage

``` r
plot_multi_compare(
  multi_compare_objects,
  plots_label = NULL,
  plot_title = NULL,
  p_value = 0.05,
  breaks = NULL,
  plot_data = FALSE,
  colors = NULL,
  variant = "one",
  p_adjust = NULL,
  note = FALSE,
  grid = "white",
  diff_perc = TRUE,
  diff_perc_size = 4.5,
  ncol_facet = 3,
  perc_diff_transparance = 0,
  diff_perc_position = "top_right",
  gradient = FALSE,
  sum_weights_indep = NULL,
  sum_weights_dep = NULL,
  label_x = NULL,
  label_y = NULL,
  missings_x = TRUE
)
```

## Arguments

- multi_compare_objects:

  A character vector containing the names of one or more
  `multi_compare_objects`. Every object will be displayed separately in
  `facet_wrap` of `ggplot`.

- plots_label:

  A character vector of the same lengths as `multi_compare_objects`, to
  name the different objects in facet_wrap of ggplot.

- plot_title:

  A string containing the title of the visualization.

- p_value:

  A number between zero and one, that is used as p-value in significance
  analyses.

- breaks:

  A vector, containing several of strings, to rename the categories in
  the legend. Its possible length depends on the `variant`.

- plot_data:

  A logical value. If `TRUE`, instead of a plot a data frame will be
  returned, that is used for the plot.

- colors:

  A vector of colors, usable in ggplot, for every break. It's possible
  length depends on the `variant`.

- variant:

  Variant can be either "one", "two", "three","four","five", or "six".

  `variant = "one"`

  :   The plot will show whether the coefficients in the regression
      models are significantly different from each other (Diff). When
      they are, it will also show if they differ in strength (one is
      twice the size of the other) or direction as well (Large Diff).

  `variant = "two"`

  :   The plot will show whether coefficients in the regression models
      differ significantly from each other (Large Diff). If not it will
      show whether they still differ in direction (Diff in Direction) or
      whether one is significant while the other is not (Diff in
      Significance).

  `variant = "three"`

  :   The plot will show whether coefficients in the regression models
      differ from each other in various aspects. Whether one is
      significant, while the other is not (Diff in Significance),
      whether they differ in direction (Diff in Direction) or whether
      one is double the size of the other (Diff in Strength). When
      variables meet the criteria for multiple categories they will
      classified in the latest fitting category.

  `variant = "four"`

  :   The plot will show if the coefficient in the df is significant,
      while the coefficient is not significant in the benchmark or the
      other way around (Diff in Significance).

  `variant = "five"`

  :   The plot will show if the coefficient in the df is positive, while
      the coefficient in the benchmark is negative or the other way
      around (Diff in Direction).

  `variant = "six"`

  :   The plot will show if the coefficient in the df is double the size
      of the coefficient in the benchmark or the other way around (Diff
      in Strength).

- p_adjust:

  If `TRUE` results based on adjusted p-values will be used. Adjustment
  methods depend on the method used to generate the
  `multi_compare_objects`.

- note:

  A logical value. If `TRUE`, a note will be displayed under the plot
  describing the `variant`.

- grid:

  A string, that can either be "none" or a color, for the edges of every
  tile. If "none", no grid will be displayed.

- diff_perc:

  A logical value. If `TRUE`, the percent of the differing categories,
  decided by the variant, will be displayed in the corner of the plot.

- diff_perc_size:

  A number to decide the size of the text in `diff_perc`.

- ncol_facet:

  A number of columns used in faced_wrap() for the plots.

- perc_diff_transparance:

  A number between zero and one, to decide the background transparency
  of `diff_perc`.

- diff_perc_position:

  A character string, to choose the position of `diff_perc` Can either
  be "top_right"(default), "bottom_right", "bottom_left", or "top_left".

- gradient:

  A logical Value. If `TRUE`, the transparency of the tiles depends on
  the coefficient.

- sum_weights_indep, sum_weights_dep:

  A vector of weights for every dependent or independent variable. Must
  be `NULL`, or the same length as the dependent variables or
  independent variables.

- label_x, label_y:

  A character string or vector of character strings containing a label
  for the x-axis and y-axis.

- missings_x:

  If `TRUE`, missing pairs in the plot will be marked with an X.

## Value

Returns a a heat matrix-like plot created with ggplot, to visualize the
multivariate differences. If multiple objects are used, they will be
displayed separately with ggplot's facet_wrap function. On the y-axis,
the independent variables are displayed, while on the x-axis the
independent variables are displayed. Depending on the variant, the
displayed tile colors must be interpreted differently. FALSEor more
information on interpretation look at `variant`.

## Examples

``` r
## Get Data for comparison

data("card")

north <- card[card$south==0,]
white <- card[card$black==0,]

## use the function to plot the data
multi_data1 <- sampcompR::multi_compare(df = north, 
                                        bench = card,
                                        independent = c("age","fatheduc","motheduc","IQ"),
                                        dependent = c("educ","wage"),
                                        family = "ols") 
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
                                     
multi_data2 <- sampcompR::multi_compare(df = white, 
                                        bench = card,
                                        independent = c("age","fatheduc","motheduc","IQ"),
                                        dependent = c("educ","wage"),
                                        family = "ols") 
#> 
#> Difference in coeficients between sets of respondents 
#>  
#>          educ         wage        
#> age      -1.20e-02    -9.51e-01   
#> fatheduc -1.45e-02    1.28e-01    
#> motheduc 9.99e-03     2.03e-01    
#> IQ       -9.28e-03    3.61e-01    
#> 
#> Overall difference between white & card: 0% of coeficients are significant different
#> (*p<0.05 ; **p<0.005 ; ***p<0.001;  for t-test robust standard errors are used) 
#>  
                                     
plot_multi_compare(c("multi_data1","multi_data2"))
#> Error in get(multi_compare_objects[i]): object 'multi_data1' not found

```
