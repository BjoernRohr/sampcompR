# sampcompR: A package for the comparison of samples

Easily analyze and visualize differences between samples (e.g.,
benchmark comparisons, nonresponse comparisons in surveys) on three
levels. The comparisons can be univariate, bivariate or multivariate. On
univariate level the variables of interest of a survey and a comparison
survey (i.e. benchmark) are compared, by calculating one of several
difference measures (e.g., relative difference in mean), and an average
difference between the surveys. On bivariate level a function can
calculate significant differences in correlations for the surveys. And
on multivariate levels a function can calculate significant differences
in model coefficients between the surveys of comparison. All of those
differences can be easily plotted and outputted as a table.
Visualization is based on
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) and can
be edited as other plots of ggplot afterwards. For more detailed
information on the methods and example use see: Rohr, B., Silber, H., &
Felderer, B. (2024). „Comparing the Accuracy of Univariate, Bivariate,
and Multivariate Estimates across Probability and Non-Probability
Surveys with Population Benchmarks“
https://doi.org/10.31235/osf.io/n6ehf.

## sampcompR functions

- [uni_compare](https://bjoernrohr.github.io/sampcompR/reference/uni_compare.md):

  Compare Datasets Univariate and Plot Differences

- [plot_uni_compare](https://bjoernrohr.github.io/sampcompR/reference/plot_uni_compare.md):

  Plot uni_compare objects

- [uni_compare_table](https://bjoernrohr.github.io/sampcompR/reference/uni_compare_table.md):

  Get a table output of a uni_compare object

- [R_indicator](https://bjoernrohr.github.io/sampcompR/reference/R_indicator.md):

  Calculate the R_indicator for several surveys

- [biv_compare](https://bjoernrohr.github.io/sampcompR/reference/biv_compare.md):

  Compare Datasets Bivariate and Plot Differences

- [plot_biv_compare](https://bjoernrohr.github.io/sampcompR/reference/plot_biv_compare.md):

  Plot biv_compare objects

- [biv_compare_table](https://bjoernrohr.github.io/sampcompR/reference/biv_compare_table.md):

  Get a table output of a biv_compare object

- [multi_compare](https://bjoernrohr.github.io/sampcompR/reference/multi_compare.md):

  Compare two Datasets on a Multivariate Level (Any GLM Model)

- [plot_multi_compare](https://bjoernrohr.github.io/sampcompR/reference/plot_multi_compare.md):

  Plot multi_compare objects

- [multi_compare_table](https://bjoernrohr.github.io/sampcompR/reference/multi_compare_table.md):

  Get a table output of multi_compare objects

- [multi_compare_merge](https://bjoernrohr.github.io/sampcompR/reference/multi_compare_merge.md):

  Combine two multi_compare objects, to plot them together

- [descriptive_table](https://bjoernrohr.github.io/sampcompR/reference/descriptive_table.md):

  Get a Descriptive Table for Every Data Frame

- [dataequalizer](https://bjoernrohr.github.io/sampcompR/reference/dataequalizer.md):

  Equalize dataframes

## uni_compare function

[uni_compare](https://bjoernrohr.github.io/sampcompR/reference/uni_compare.md)
Returns data or a plot showing the difference of two or more data frames
The differences are calculated on the base of differing metrics, chosen
in the funct argument. Results can be visualized using
[`plot_uni_compare`](https://bjoernrohr.github.io/sampcompR/reference/plot_uni_compare.md).

## biv_compare function

[biv_compare](https://bjoernrohr.github.io/sampcompR/reference/biv_compare.md)
Returns data or heatmap of difference between two or more data frames,
by comparing their correlation matrices. The comparison is based on
Pearson's r, calculated using the
[`rcorr`](https://rdrr.io/pkg/Hmisc/man/rcorr.html) function. Results
can be visualized using
[`plot_biv_compare`](https://bjoernrohr.github.io/sampcompR/reference/plot_biv_compare.md).

## multi_compare function

[multi_compare](https://bjoernrohr.github.io/sampcompR/reference/multi_compare.md)
Returns data of difference between two data frames on a multivariate
level. Similar (multivariate) regression models are compared between the
surveys. Only GLM models are possible. Results can be visualized using
[`plot_multi_compare`](https://bjoernrohr.github.io/sampcompR/reference/plot_multi_compare.md).

## dataequalizer function

[dataequalizer](https://bjoernrohr.github.io/sampcompR/reference/dataequalizer.md)
compares two data frames and looks if data frames contain columns with
the same name. A copy of y is returned, containing only columns named
identical in x and y data frames. The function is mainly used in the
other functions of the package.

\_PACKAGE
