# card

This data, which originates from D. Card (1995) was released in the
Wooldridge R-Package. Sadly the wooldridge package (Shea 2023) was
archived on CRAN on the 3rd of December 2024. As we use it, e.g., in our
examples to show how our package works, we also added it to our package,
so we can further use it. Further we cite the original description of
the wooldrigde package. Wooldridge Source: D. Card (1995), Using
Geographic Variation in College Proximity to Estimate the Return to
Schooling, in Aspects of Labour Market Behavior: Essays in Honour of
John Vanderkamp. Ed. L.N. Christophides, E.K. Grant, and R. Swidinsky,
201-222. Toronto: University of Toronto Press. Professor Card kindly
provided these data. Data loads lazily.

## Usage

``` r
data(card)
```

## Format

A data.frame with 3010 observations on 34 variables:

- **id:** person identifier

- **nearc2:** =1 if near 2 yr college, 1966

- **nearc4:** =1 if near 4 yr college, 1966

- **educ:** years of schooling, 1976

- **age:** in years

- **fatheduc:** father's schooling

- **motheduc:** mother's schooling

- **weight:** NLS sampling weight, 1976

- **momdad14:** =1 if live with mom, dad at 14

- **sinmom14:** =1 if with single mom at 14

- **step14:** =1 if with step parent at 14

- **reg661:** =1 for region 1, 1966

- **reg662:** =1 for region 2, 1966

- **reg663:** =1 for region 3, 1966

- **reg664:** =1 for region 4, 1966

- **reg665:** =1 for region 5, 1966

- **reg666:** =1 for region 6, 1966

- **reg667:** =1 for region 7, 1966

- **reg668:** =1 for region 8, 1966

- **reg669:** =1 for region 9, 1966

- **south66:** =1 if in south in 1966

- **black:** =1 if black

- **smsa:** =1 in in SMSA, 1976

- **south:** =1 if in south, 1976

- **smsa66:** =1 if in SMSA, 1966

- **wage:** hourly wage in cents, 1976

- **enroll:** =1 if enrolled in school, 1976

- **KWW:** knowledge world of work score

- **IQ:** IQ score

- **married:** =1 if married, 1976

- **libcrd14:** =1 if lib. card in home at 14

- **exper:** age - educ - 6

- **lwage:** log(wage)

- **expersq:** exper^2

## Source

<https://www.cengage.com/cgi-wadsworth/course_products_wp.pl?fid=M20b&product_isbn_issn=9781111531041>

## Notes

Computer Exercise C15.3 is important for analyzing these data. There, it
is shown that the instrumental variable, `nearc4`, is actually
correlated with `IQ`, at least for the subset of men for which an IQ
score is reported. However, the correlation between
``` nearc4`` and  ```IQ`, once the other explanatory variables are netted out, is arguably zero. At least, it is not statistically different from zero. In other words, `nearc4\`
fails the exogeneity requirement in a simple regression model but it
passes, at least using the crude test described above, if controls are
added to the wage equation. For a more advanced course, a nice extension
of Card's analysis is to allow the return to education to differ by
race. A relatively simple extension is to include black education
(blackeduc) as an additional explanatory variable; its natural
instrument is blacknearc4.

Used in Text: pages 526-527, 547

## References

Shea J (2023). *wooldridge: 115 Data Sets from "Introductory
Econometrics: A Modern Approach, 7e" by Jeffrey M. Wooldridge*. R
package version 1.4-3, <https://CRAN.R-project.org/package=wooldridge>.

## Examples

``` r
 data("card")
str(card)
#> 'data.frame':    3010 obs. of  34 variables:
#>  $ id      : int  2 3 4 5 6 7 8 9 10 11 ...
#>  $ nearc2  : int  0 0 0 1 1 1 1 1 1 1 ...
#>  $ nearc4  : int  0 0 0 1 1 1 1 1 1 1 ...
#>  $ educ    : int  7 12 12 11 12 12 18 14 12 12 ...
#>  $ age     : int  29 27 34 27 34 26 33 29 28 29 ...
#>  $ fatheduc: int  NA 8 14 11 8 9 14 14 12 12 ...
#>  $ motheduc: int  NA 8 12 12 7 12 14 14 12 12 ...
#>  $ weight  : num  158413 380166 367470 380166 367470 ...
#>  $ momdad14: int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ sinmom14: int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ step14  : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ reg661  : int  1 1 1 0 0 0 0 0 0 0 ...
#>  $ reg662  : int  0 0 0 1 1 1 1 1 1 1 ...
#>  $ reg663  : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ reg664  : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ reg665  : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ reg666  : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ reg667  : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ reg668  : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ reg669  : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ south66 : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ black   : int  1 0 0 0 0 0 0 0 0 0 ...
#>  $ smsa    : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ south   : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ smsa66  : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ wage    : int  548 481 721 250 729 500 565 608 425 515 ...
#>  $ enroll  : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ KWW     : int  15 35 42 25 34 38 41 46 32 34 ...
#>  $ IQ      : int  NA 93 103 88 108 85 119 108 96 97 ...
#>  $ married : int  1 1 1 1 1 1 1 1 4 1 ...
#>  $ libcrd14: int  0 1 1 1 0 1 1 1 0 1 ...
#>  $ exper   : int  16 9 16 10 16 8 9 9 10 11 ...
#>  $ lwage   : num  6.31 6.18 6.58 5.52 6.59 ...
#>  $ expersq : int  256 81 256 100 256 64 81 81 100 121 ...
#>  - attr(*, "time.stamp")= chr "25 Jun 2011 23:03"
```
