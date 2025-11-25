# Equalize dataframes

`dataequalizer` compares two data frames and looks if both data frames
contain columns with the same Name. A copy of source_df is returned,
containing only columns named identical in target_df and source_df data
frames. The function is mainly used in the other functions of the
package.

## Usage

``` r
dataequalizer(target_df, source_df, variables = NULL, silence = FALSE)
```

## Arguments

- target_df:

  A data frame

- source_df:

  A data frame containing some column-names named equally in target_df

- variables:

  A vector to indicate variable names that should be in the copy of the
  source_df if they are also in the target_df.

- silence:

  A logic value. If FALSE, warnings will be returned indicating, what
  variables where removed, from the survey.

## Value

Returns a copy of source_df containing only variables with names
contained also in the target_df data frame.

## Examples

``` r
## Get Data to equalize 
data("card")

##reduce data frame
card2<-card[c("id","age","educ","fatheduc","motheduc","IQ","wage")]

card_equalized<-sampcompR::dataequalizer(card2,card,variables=c("age","educ","IQ","wage"))
#> Warning: Only chosen variables included in both datasets are used 
#>    Missing variables are: id | nearc2 | nearc4 | fatheduc | motheduc | weight | momdad14 | sinmom14 | step14 | reg661 | reg662 | reg663 | reg664 | reg665 | reg666 | reg667 | reg668 | reg669 | south66 | black | smsa | south | smsa66 | enroll | KWW | married | libcrd14 | exper | lwage | expersq
card_equalized[1:20,]
#>    age educ  IQ wage
#> 1   29    7  NA  548
#> 2   27   12  93  481
#> 3   34   12 103  721
#> 4   27   11  88  250
#> 5   34   12 108  729
#> 6   26   12  85  500
#> 7   33   18 119  565
#> 8   29   14 108  608
#> 9   28   12  96  425
#> 10  29   12  97  515
#> 11  28    9  84  225
#> 12  26   12  89  400
#> 13  24   11  93  417
#> 14  30   11  74  217
#> 15  31   16 116  894
#> 16  24   14  NA  300
#> 17  34   12  93  346
#> 18  29   14 100  658
#> 19  26   10  91  575
#> 20  32   12  88  649
```
