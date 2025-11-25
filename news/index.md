# Changelog

## sampcompR (development version)

### Changes in version 0.3.2 (2025-25-11)

- Fixed calculation of standard error of the R-Indicator in the
  R_indicator Function
- Changed to calculate R-Indicator based on family = binomial(“logit)”
  instead of quasibinomial(“logit”)

### Changes in version 0.3.1.2 (2025-03-07)

- Fixed a small bug where multivariate biases got the wrong color, when
  one type of bias was not present

### Changes in version 0.3.1.1 (2025-02-07)

- Fixed a small bug so univariate and bivariate bias functions also work
  with factor variables

### Changes in version 0.3.1 (2025-13-03)

- Added two parameter to plot_uni_compare to adjust the size of the dots
  and the errorbars

### Changes in version 0.3.0 (2025-02-03)

- fix small bug in shapes of plot_uni_compare
- add a new function, to create a heatmap of relative bias for bivariate
  comparison
- add a new function, to show AAB or AARB in Pearson’s r per variable as
  a table
- add a new function, to calculate missings per variable and display
  them in a table

### Changes in version 0.2.7 (2025-21-01)

- fixed a small error in biv_compare that occurred when sample size was
  very small and ended in an Error message.
- Added absolute relative bias to biv_compare output

### Changes in version 0.2.6 (2024-14-11)

- As the wooldridge package was archived on CRAN, and our examples rely
  on the card data of wooldridge, we added the card data to our package

### Changes in version 0.2.5 (2024-14-11)

- Added the possibility to input benchmarks as a named vector of means
  in univariate comparison
- Fixed small errors in the uni_compare_table functions that occurred
  when only one benchmark and survey were compared.
- Changed example to show bias comparison (estimating bias if only white
  respondents or if only north respondents would have been recruited)

### Changes in version 0.2.4 (2024-10-11)

- Added parameters that allow to bootstrap both benchmark and survey
- Added parameter to allow to choose between percentile or normal
  bootstrap confidence intervals and p-values based on those intervals.

### Changes in version 0.2.3 (2024-19-08)

- Additional small Fix for a test

### Changes in version 0.2.2 (2024-22-07)

- Small Fix for test on CRAN for MacOS

### Changes in version 0.2.1 (2024-14-07)

- We added biv_per_variable() a function to calculate the average bias
  per variable for the bivariate comparison, and an average bias per
  variable across comparisons.

- We added multi_per_variable() a function to calculate the average bias
  per coefficient and per model, for the multivariate comparison, and an
  average biases per coefficient and per model across comparisons.

### Changes in version 0.2.0 (2024-08-07)

- We implemented better bootstrapping, that will use weighting in every
  bootstrap iteration, for all main functions (, , .

- The functions are much faster during bootstrapping now.

- The possibility to weight the dataset to the benchmark using and .
