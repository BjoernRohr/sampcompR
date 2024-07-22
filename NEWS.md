# sampcompR (development version)

* Initial CRAN submission.

## Changes in version 0.2.2 (2024-22-07)

* Small Fix for test on CRAN for MacOS

## Changes in version 0.2.1 (2024-14-07)

* We added biv_per_variable() a function to calculate the average bias per variable for the bivariate comparison,
  and an average bias per variable across comparisons.

* We added multi_per_variable() a function to calculate the average bias per coefficient and per model, 
  for the multivariate comparison, and an average biases per coefficient and per model across comparisons.


## Changes in version 0.2.0 (2024-08-07)

* We implemented better bootstrapping, that will use weighting in every bootstrap iteration, 
  for all main functions (\code{uni_compare}, \code{biv_compare}, \code{multi_compare}.

* The functions are much faster during bootstrapping now.

* The possibility to weight the dataset to the benchmark using \code{raking} and \code{post-stratification}.