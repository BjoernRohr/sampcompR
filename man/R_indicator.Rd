% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Unidiff_Plot.R
\name{R_indicator}
\alias{R_indicator}
\title{Calculate the R-Indicator}
\usage{
R_indicator(
  dfs,
  response_identificators,
  variables,
  id = NULL,
  weight = NULL,
  strata = NULL,
  get_r2 = FALSE
)
}
\arguments{
\item{dfs}{A character vector containing the names of data frames
to calculate the R indicator.}

\item{response_identificators}{A character vector, naming response identificators
for every df. Response identificators should indicate if respondents are part
of the set of respondents \code{(respondents = 1)} or not part of the set of
respondents.
\code{(non-respondents = 0)}. If only one character is provided, the same
variable is used in every df.}

\item{variables}{A character vector with the names of variables that should be
used in the model to calculate the R indicator.}

\item{id}{A character vector that determines id variables that are used to
weight the dfs with the help of the survey package. They have to be part of
the respective data frame. If only one character is provided, the same
variable is used to weight every df.}

\item{weight}{A character vector that determines variables to weight the dfs.
They have to be part of the respective data frame. If only one character is
provided, the same variable used to weight every df. If a weight variable is
provided also an id variable is needed. For weighting, the survey package is
used.}

\item{strata}{A character vector that determines strata variables that are
used to weight the dfs with the help of the survey package. They have to
be part of the respective data frame. If only one character is provided,
the same variable is used to weight every df.}

\item{get_r2}{If true, Pseudo R-squared of the propensity model will be
returned, based on the method of McFadden.}
}
\value{
A list containing the R-indicator, and its standard error for every data frame.
}
\description{
Calculates the R-Indicator of the (weighted) data frame.
}
\note{
The calculated R-indicator is based on Shlomo et al., (2012).
}
\examples{


data("card")

# For the purpose of this example, we assume that only respondents living in 
# the south or only white respondents have participated in the survey.

sampcompR::R_indicator(dfs=c("card","card"),
                       response_identificators = c("south","black"),
                       variables = c("age","educ","fatheduc","motheduc","wage","IQ"),
                       weight = c("weight","weight"))

}
\references{
\itemize{
\item Shlomo, N., Skinner, C., & Schouten, B. (2012). Estimation of an
indicator of the representativeness of survey response. Journal of Statistical
Planning and Inference, 142(1), 201–211. https://doi.org/10.1016/j.jspi.2011.07.008
}
}
