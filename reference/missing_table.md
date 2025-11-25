# Returns a Table indicating the number and proportion of NA values for a selected set of variables.

Returns a Table indicating the number and proportion of NA values for a
selected set of variables.

## Usage

``` r
missing_table(dfs, variables, df_names = NULL, varlabels = NULL)
```

## Arguments

- dfs:

  A character vector with names of data frames for which the missings
  per variable should be displayed.

- variables:

  A character vector of variable names for which the missings should be
  displyed.

- df_names:

  Either Null or a character vector of names, to relabel the data frames
  in the table with.

- varlabels:

  Either Null, or a character vector of variable names, to relabel the
  variables in the table with.

## Value

Returns a Table indicating the number and proportion of NA values for a
selected set of variables. This can be used to get an overview of the
data, detect errors after data rangeling, or find items in a survey,
with especially, high item nonresponse.

## Examples

``` r
## Get Data for comparison

data("card")

north <- card[card$south==0,]
white <- card[card$black==0,]

variables<- c("age","educ","fatheduc","motheduc","wage","IQ")
varlabels<-c("Age","Education","Father's Education",
             "Mother's Education","Wage","IQ")

missing_tab<-sampcompR::missing_table(dfs = c("north","white"),
                                      variables=variables,
                                      df_names = c("North","White"),
                                      varlabels=varlabels)
#> Error in purrr::map(dfs, get): ℹ In index: 1.
#> Caused by error in `.f()`:
#> ! object 'north' not found

missing_tab
#> Error: object 'missing_tab' not found


```
