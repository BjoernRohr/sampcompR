# Returns a table based on the information of a `biv_compare_object` that indicates the Average Absolute Bias (AARB) in Pearson's r or the Average Absolute Relative Bias (AARB) in Pearson's r for every data frame It can be outputted as HTML or LaTex Table, for example with the help of the [stargazer](https://rdrr.io/pkg/stargazer/man/stargazer.html) function.

Returns a table based on the information of a `biv_compare_object` that
indicates the Average Absolute Bias (AARB) in Pearson's r or the Average
Absolute Relative Bias (AARB) in Pearson's r for every data frame It can
be outputted as HTML or LaTex Table, for example with the help of the
[stargazer](https://rdrr.io/pkg/stargazer/man/stargazer.html) function.

## Usage

``` r
biv_bias_per_variable(
  biv_compare_object,
  type = "rel_diff",
  final_col = "difference",
  ndigits = 3,
  varlabels = NULL,
  label_df = NULL
)
```

## Arguments

- biv_compare_object:

  A object returned by the
  [`biv_compare`](https://bjoernrohr.github.io/sampcompR/reference/biv_compare.md)
  function.

- type:

  A character string, which is `"AAB"` if the Average Absolute Bias per
  variable should be displayed in the table, or "AARB" if the Average
  Absolute Relative Bias per Variable should be displayed in the table.

- final_col:

  A character string, indicating if the last column of the table should
  display an average bias per variable of over all data frames
  (`"average"`), or the difference between the first and the average
  bias of the first and the last data frame (`"difference"`).

- ndigits:

  Number of digits that is shown in the table.

- varlabels:

  A character vector containing labels for the variables.

- label_df:

  A character vector containing labels for the data frames.

## Value

A matrix, that shows the Average Absolute Bias (AAB) or the Average
Absolute Relative Bias (AARB) for every individual variable. This is
given separately for every comparison data frame, as well as averaged
over comparisons, or as the difference between the first and the last
comparison.

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

table1<-sampcompR::biv_bias_per_variable(bivar_data,type="rel_diff",
                                         final_col="average",ndigits=2)
noquote(table1)

table2<-sampcompR::biv_bias_per_variable(bivar_data,type = "diff",
                                         final_col="difference",ndigits=2)
noquote(table2)} # }
```
