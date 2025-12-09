# Returns a table based on the information of a `biv_compare_object` that indicates the proportion of biased variables. It can be outputted as HTML or LaTex Table, for example with the help of the [stargazer](https://rdrr.io/pkg/stargazer/man/stargazer.html) function.

Returns a table based on the information of a `biv_compare_object` that
indicates the proportion of biased variables. It can be outputted as
HTML or LaTex Table, for example with the help of the
[stargazer](https://rdrr.io/pkg/stargazer/man/stargazer.html) function.

## Usage

``` r
biv_per_variable(
  biv_compare_object,
  ndigits = 1,
  varlabels = NULL,
  label_df = NULL
)
```

## Arguments

- biv_compare_object:

  A object returned by the
  [`biv_compare`](https://bjoernrohr.github.io/sampcompR/reference/biv_compare.md)
  function.

- ndigits:

  Number of digits that is shown in the table.

- varlabels:

  A character vector containing labels for the variables.

- label_df:

  A character vector containing labels for the data frames.

## Value

A matrix, that indicates the proportion of bias for every individual
variable. This is given separately for every comparison, as well as
averaged over comparisons.

## Examples

``` r
if (FALSE) { # \dontrun{
data("card")

north <- card[card$south==0,]
white <- card[card$black==0,]

## use the function to plot the data 
bivar_data<-sampcompR::biv_compare(dfs = c("north","white"),
                                   benchmarks = c("card","card"),
                                   variables= c("age","educ","fatheduc","motheduc","wage","IQ"),
                                   data=TRUE)

table<-sampcompR::biv_per_variable(bivar_data)
noquote(table)} # }
```
