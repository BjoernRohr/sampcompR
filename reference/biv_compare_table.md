# Returns a table based on the information of a `biv_compare_object` which can be outputted as HTML or LaTex Table, for example with the help of the [stargazer](https://rdrr.io/pkg/stargazer/man/stargazer.html) function.

Returns a table based on the information of a `biv_compare_object` which
can be outputted as HTML or LaTex Table, for example with the help of
the [stargazer](https://rdrr.io/pkg/stargazer/man/stargazer.html)
function.

## Usage

``` r
biv_compare_table(
  biv_compare_object,
  type = "diff",
  comparison_number = 1,
  ndigits = 2
)
```

## Arguments

- biv_compare_object:

  A object returned by the
  [`biv_compare`](https://bjoernrohr.github.io/sampcompR/reference/biv_compare.md)
  function.

- type:

  A character string, to choose what matrix should be printed.

  - If "dfs", a correlation matrix of all variables of comparison in the
    chosen dataframe will be returned.

  - If "benchmarks", a correlation matrix of all variables of comparison
    in the chosen benchmark will be returned.

  - if "diff", a matrix indicating the difference between the chosen
    dataframe and benchmark will be returned.

- comparison_number:

  A number indicating the data of which data frame, benchmark or
  comparison should be displayed. The maximum length is equal to the
  length of the `dfs vector` that is used to generate the
  `biv_compare_object`.

- ndigits:

  Number of digits shown in the table.

## Value

A correlation matrix, or difference matrix based on information of a
`biv_compare_object`.

## Examples

``` r
## Get Data for comparison

if (FALSE) { # \dontrun{
data("card")

north <- card[card$south==0,]
white <- card[card$black==0,]

## use the function to plot the data 
bivar_data<-sampcompR::biv_compare(dfs = c("north","white"),
                                   benchmarks = c("card","card"),
                                   variables= c("age","educ","fatheduc","motheduc","wage","IQ"),
                                   data=TRUE)
                        
table<-sampcompR::biv_compare_table(bivar_data, type="diff", comparison_number=1)
noquote(table)} # }

```
