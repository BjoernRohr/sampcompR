# Combine multi_compare_objects

`multi_compare_merge` combines two `multi_compare_objects` to plot them
together.

## Usage

``` r
multi_compare_merge(multi_reg_object1, multi_reg_object2, p_adjust = FALSE)
```

## Arguments

- multi_reg_object1, multi_reg_object2:

  Multireg objects that should be combined.

- p_adjust:

  A logical input or character string indicating an adjustment method
  that isusable in the `method` parameter of
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html). If set to TRUE
  the Bonferroni adjusted p-values are used in inference.

## Value

A combined `multi_reg_object` that can be used in plot functions to
create a visualization.

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
                                        dependent = c("educ"),
                                        family = "ols") 
#> 
#> Difference in coeficients between sets of respondents 
#>  
#>          educ        
#> age      -2.43e-02   
#> fatheduc -2.37e-02   
#> motheduc 1.23e-02    
#> IQ       -7.25e-03   
#> 
#> Overall difference between north & card: 0% of coeficients are significant different
#> (*p<0.05 ; **p<0.005 ; ***p<0.001;  for t-test robust standard errors are used) 
#>  
                                     
multi_data2 <- sampcompR::multi_compare(df = white, 
                                        bench = card,
                                        independent = c("age","fatheduc","motheduc","IQ"),
                                        dependent = c("wage"),
                                        family = "ols") 
#> 
#> Difference in coeficients between sets of respondents 
#>  
#>          wage        
#> age      -9.51e-01   
#> fatheduc 1.28e-01    
#> motheduc 2.03e-01    
#> IQ       3.61e-01    
#> 
#> Overall difference between white & card: 0% of coeficients are significant different
#> (*p<0.05 ; **p<0.005 ; ***p<0.001;  for t-test robust standard errors are used) 
#>  
 ### merge two objects ###                                       
 merged_object<-multi_compare_merge(multi_data1,multi_data2)
 
 ### Plot the merged object ###
 plot_multi_compare("merged_object")                                       
#> Error in get(multi_compare_objects[i]): object 'merged_object' not found
```
