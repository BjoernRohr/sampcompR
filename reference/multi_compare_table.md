# Create an Output-Table of a multi_compare_object

Returns a table based on the information of a `multi_compare_object`
which can be outputted as HTML or LaTex Table, for example with the help
of the [stargazer](https://rdrr.io/pkg/stargazer/man/stargazer.html)
function.

## Usage

``` r
multi_compare_table(
  multi_compare_objects,
  type = "diff",
  names = NULL,
  ndigits = 3,
  envir = parent.frame()
)
```

## Arguments

- multi_compare_objects:

  One or more object that were returned by
  [`multi_compare`](https://bjoernrohr.github.io/sampcompR/reference/multi_compare.md).

- type:

  A character string, to determine the type of regression table.

  - If "dfs" a regression table based on the data frame(s) is returned.

  - If "benchmarks" a regression table based on the benchmark(s) is
    returned.

  - If "diff" a table indicating the difference between the df(s) and
    the benchmark(s) is returned.

- names:

  A character vector to rename the data frames of comparison.

- ndigits:

  The Number of digits that is shown in the table.

- envir:

  The environment, where the `multi_core_objects` can be found.

## Value

A table containing information on the multivariate comparison based on
the
[`multi_compare`](https://bjoernrohr.github.io/sampcompR/reference/multi_compare.md)
function.

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
                                     
table<-multi_compare_table(c("multi_data1","multi_data2"),type="diff")

noquote(table)
#>       data_frames variables educ       wage      
#>  [1,] north       age       -0.024     -0.855    
#>  [2,]                       (0.025)    (3.257)   
#>  [3,]             fatheduc  -0.024     -0.293    
#>  [4,]                       (0.028)    (3.682)   
#>  [5,]             motheduc    0.012      2.350   
#>  [6,]                       (0.033)    (4.379)   
#>  [7,]             IQ        -0.007       0.480   
#>  [8,]                       (0.005)    (0.691)   
#>  [9,] white       age       -0.012     -0.951    
#> [10,]                       (0.023)    (3.02)    
#> [11,]             fatheduc  -0.014       0.128   
#> [12,]                       (0.025)    (3.287)   
#> [13,]             motheduc    0.010      0.203   
#> [14,]                       (0.029)    (3.915)   
#> [15,]             IQ        -0.009       0.361   
#> [16,]                       (0.005)    (0.652)   
```
