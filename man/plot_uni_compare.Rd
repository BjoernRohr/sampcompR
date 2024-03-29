% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Unidiff.R
\name{plot_uni_compare}
\alias{plot_uni_compare}
\title{plot univar data}
\usage{
plot_uni_compare(
  uni_compare_objects,
  name_dfs = NULL,
  name_benchmarks = NULL,
  summetric = NULL,
  colors = NULL,
  shapes = NULL,
  legendlabels = NULL,
  legendtitle = NULL,
  label_x = NULL,
  label_y = NULL,
  summet_size = NULL,
  plot_title = NULL,
  conf_adjustment = F,
  varlabels = NULL
)
}
\arguments{
\item{uni_compare_objects}{A object generated by \code{\link[sampcompR]{uni_compare}}
function.}

\item{name_dfs, name_benchmarks}{A character string or vector of character strings containing the
new names of the data frames and benchmarks, also used in plot.}

\item{summetric}{If ,\code{"avg"}, \code{"mse1"}, \code{"rmse1"}, or \code{"R"}
the respective measure is calculated for the biases of each survey. The values
\code{"mse"} and \code{"rmse2"} lead to similar results as in \code{"mse1"} and \code{"rmse1"},
with slightly different visualization in the plot. If summetric = NULL, no summetric
will be displayed in the Plot.}

\item{colors}{A vector of colors used in the plot for the
different comparisons.}

\item{shapes}{A vector of shapes applicable in \code{\link[ggplot2:ggplot2-package]{ggplot2::ggplot2()}} used in the plot for the different
comparisons.}

\item{legendlabels}{A character string or vector of strings containing a label for the
legend.}

\item{legendtitle}{A character string containing the title of the Legend.}

\item{label_x, label_y}{A character string or vector of character strings containing a label for
the x-axis and y-axis.}

\item{summet_size}{A number to determine the size of the displayed summetric in the plot.}

\item{plot_title}{A character string containing the title of the plot.}

\item{conf_adjustment}{If conf_adjustment=T the cofidence level of the confidence interval will be
adjusted with a bonferoni adjustment, to account for the problem of multiple comparisons.}

\item{varlabels}{A character string or vector of character strings containing the new names of
variables, also used in plot.}
}
\value{
Plot of a \code{\link[sampcompR]{uni_compare}} object using
\code{\link[ggplot2:ggplot2-package]{ggplot2::ggplot2()}} which shows the difference between two or more data frames.
}
\description{
\code{plot_uni_compare} This uses ggplot2 to generate a plot based on an object
generated by the \code{\link[sampcompR]{uni_compare}} function.
}
\examples{

## Get Data for comparison
card<-wooldridge::card

south <- card[card$south==1,]
north <- card[card$south==0,]
black <- card[card$black==1,]
white <- card[card$black==0,]

## use the function to plot the data 
univar_data<-sampcompR::uni_compare(dfs = c("north","white"),
                                    benchmarks = c("south","black"),
                                    variables= c("age","educ","fatheduc","motheduc","wage","IQ"),
                                    funct = "abs_rel_mean",
                                    nboots=0,
                                    summetric="rmse2",
                                    data=TRUE)

sampcompR::plot_uni_compare(univar_data)

}
