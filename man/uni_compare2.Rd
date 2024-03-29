% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Unidiff2.R
\name{uni_compare2}
\alias{uni_compare2}
\title{Compare data frames and Plot Differences}
\usage{
uni_compare2(
  dfs,
  benchmarks,
  variables = NULL,
  nboots = 2000,
  funct = "rel_mean",
  data = FALSE,
  type = "comparison",
  legendlabels = NULL,
  legendtitle = NULL,
  colors = NULL,
  shapes = NULL,
  summetric = "rmse2",
  label_x = NULL,
  label_y = NULL,
  plot_title = NULL,
  varlabels = NULL,
  name_dfs = NULL,
  name_benchmarks = NULL,
  summet_size = 4,
  ci_type = "perc",
  silence = T,
  conf_level = 0.95,
  conf_adjustment = NULL,
  weight = NULL,
  id = NULL,
  strata = NULL,
  weight_bench = NULL,
  id_bench = NULL,
  strata_bench = NULL
)
}
\arguments{
\item{dfs}{A character vector containing the names of data frames to compare against the benchmarks.}

\item{benchmarks}{A character vector containing the names of benchmarks to compare the data frames against.
The vector must either to be the same length as \code{dfs}, or length 1. If it has length 1 every
df will be compared against the same benchmark. Benchmarks can either be the name of data frames or
the name of a list of tables. The tables in the list need to be named as the respective variables
in the data frame of comparison.}

\item{variables}{A character vector containing the names of the variables for the comparison. If NULL,
all variables named similar in both the dfs and the benchmarks will be compared. Variables missing
in one of the data frames or the benchmarks will be neglected for this comparison.}

\item{nboots}{The Number of bootstraps used to calculate standard errors. Must either be >2 or 0.
If >2 bootstrapping is used to calculate standard errors with \code{nboots} iterations. If 0, SE
is calculated analytically. We do not recommend using \code{nboots} =0 because this method is not
yet suitable for every \code{funct} used and every method. Depending on the size of the data and the
number of bootstraps, \code{uni_compare2} can take a while.}

\item{funct}{A function or a vector of functions to calculate the difference between the
data frames. If a single input is given, the same function will be used for all variables.
If the input is a vector, the vector has to be of the same length as \code{variables}.
Then for each variable the indicated function will be used. The input can either be a
character indicating a predefined function, or function (not yet clearly defined).
Predefined functions are:
\itemize{
\item \code{"d_mean"}, \code{"ad_mean"} A function to calculate the (absolute) difference in mean of
the variables in dfs and benchmarks with the same name. Only applicable for
metric variables
\item \code{"d_prop"}, \code{"ad_prop"} A function to calculate the (absolute) difference in proportions of
the variables in dfs and benchmarks with the same name. Only applicable for dummy
variables.
\item \code{"rel_mean"}, \code{"abs_rel_mean"} A function to calculate the (absolute)
relative difference in mean of the variables in dfs and benchmarks with the same name.
#' For more information on the formula for difference and analytic variance, see Felderer
et al. (2019). Only applicable for metric variables
\item \code{"rel_prop"}, \code{"abs_rel_prop"} A function to calculate the (absolute)
relative difference in proportions of the variables in dfs and benchmarks with
the same name. it is calculated similar to the relative difference in mean
(see Felderer et al., 2019), however the default label for the plot is different.
Only applicable for dummy variables.
\item \code{"ad_median"} A function to calculate the (absolute) relative difference in median of
the variables in dfs and benchmarks with the same name.
}}

\item{data}{If TRUE, a uni_compare_object is returned, containing results of the comparison.}

\item{type}{Define the type of comparison. Can either be "comparison" or "nonrespnse".}

\item{legendlabels}{A character string or vector of strings containing a label for the
legend.}

\item{legendtitle}{A character string containing the title of the Legend.}

\item{colors}{A vector of colors used in the plot for the
different comparisons.}

\item{shapes}{A vector of shapes applicable in \code{\link[ggplot2:ggplot2-package]{ggplot2::ggplot2()}} used in the plot for
the different comparisons.}

\item{summetric}{If ,\code{"avg"}, \code{"mse1"}, \code{"rmse1"}, or \code{"R"}
the respective measure is calculated for the biases of each survey. The values
\code{"mse1"} and \code{"rmse2"} lead to similar results as in \code{"mse1"} and \code{"rmse1"},
with slightly different visualization in the plot. If summetric = NULL, no summetric
will be displayed in the Plot. When \code{"R"} is chosen, also \code{response_identificator}
is needed.}

\item{label_x, label_y}{A character string or vector of character strings containing a label for
the x-axis and y-axis.}

\item{plot_title}{A character string containing the title of the plot.}

\item{varlabels}{A character string or vector of character strings containing the new names of
variables, also used in plot.}

\item{name_dfs, name_benchmarks}{A character string or vector of character strings containing the
new names of the data frames and benchmarks, also used in plot.}

\item{summet_size}{A number to determine the size of the displayed summetric in the plot.}

\item{ci_type}{A character string determining the type of bootstrap ci available in the
\code{\link[boot]{boot.ci}} function of the code{boot} package.}

\item{silence}{If silence = F a warning will be displayed, if variables are excluded from either
data frame or benchmark, for not existing in both.}

\item{conf_level}{A numeric value between zero and one to determine the confidence level of the confidence
interval.}

\item{conf_adjustment}{If conf_adjustment=T the confidence level of the confidence interval will be
adjusted with a Bonferroni adjustment, to account for the problem of multiple comparisons.}

\item{weight, weight_bench}{A character vector determining variables to weight the \code{dfs} or
\code{benchmarks}. They have to be part of the respective data frame. If only one character is provided,
the same variable is used to weight every df or benchmark. If a weight variable is provided also an id
variable is needed.For weighting, the \code{survey} package is used.}

\item{id, id_bench}{A character vector determining id variables used to weight the \code{dfs} or
\code{benchmarks} with the help of the \code{survey} package. They have to be part of the respective
data frame. If only one character is provided, the same variable is used to weight every df or benchmark.}

\item{strata, strata_bench}{A character vector determining strata variables used to weight
the \code{dfs} or \code{benchmarks} with the help of the \code{survey} package. They have
to be part of the respective data frame. If only one character is provided, the same variable
is used to weight every df or benchmark.}
}
\value{
A plot based on \code{\link[ggplot2:ggplot2-package]{ggplot2::ggplot2()}} (or data frame if data==TRUE)
which shows the difference between two or more data frames on predetermined variables,
named identical in both data frames.
}
\description{
Returns a plot or data showing the difference of two or more
data frames The differences are calculated on the base of
differing metrics, chosen in the funct argument. All used data frames must
contain at least one column named equal in all data frames, that has equal
values. It works similar as \link[sampcompR]{uni_compare}, however it's bootstrap technique
is based on the \link[survey]{svydesign} package, making it more suitable for the comparison of weighted data.
}
\examples{

## Get Data for comparison
card<-wooldridge::card

black<-wooldridge::card[wooldridge::card$black==1,]
north<-wooldridge::card[wooldridge::card$south==0,]
white<-wooldridge::card[wooldridge::card$black==0,]
south<-wooldridge::card[wooldridge::card$south==1,]

## use the function to plot the data 
univar_comp<-sampcompR::uni_compare2(dfs = c("north","white"),
                                    benchmarks = c("south","black"),
                                    variables= c("age","educ","fatheduc","motheduc","wage","IQ"),
                                    funct = "abs_rel_mean",
                                    nboots=200,
                                    summetric="rmse2",
                                    data=FALSE)

 univar_comp
 
}
\references{
Felderer, B., Kirchner, A., & Kreuter, F. (2019). The Effect of Survey Mode on Data
Quality: Disentangling Nonresponse and Measurement Error Bias. Journal of Official
Statistics, 35(1), 93–115. https://doi.org/10.2478/jos-2019-0005
}
