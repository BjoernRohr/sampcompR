# Create an Output-Table of a uni_compare_object

Returns a table based on the information of an `uni_compare_object`
which can be outputted as HTML or LaTex Table, for example with the help
of the [stargazer](https://rdrr.io/pkg/stargazer/man/stargazer.html)
function.

## Usage

``` r
uni_compare_table(
  uni_compare_object,
  conf_adjustment = FALSE,
  df_names = NULL,
  varlabels = NULL,
  ci_line = TRUE,
  ndigits = 3
)
```

## Arguments

- uni_compare_object:

  A object returned by
  [`uni_compare`](https://bjoernrohr.github.io/sampcompR/reference/uni_compare.md).

- conf_adjustment:

  A logical parameter determining if adjusted confidence intervals
  should be returned.

- df_names:

  A character vector to relabel the data frames of comparison.

- varlabels:

  A character vector to relabel the variables in the table.

- ci_line:

  If `TRUE`, confidence intervals will be displayed in a separate line,
  otherwise, they are shown in the same line instead.

- ndigits:

  The number of digits to round the numbers in table.

## Value

A table containing information on the univariate comparison based on the
[`uni_compare`](https://bjoernrohr.github.io/sampcompR/reference/uni_compare.md)
function.

## Examples

``` r
## Get Data for comparison

data("card")

north <- card[card$south==0,]
white <- card[card$black==0,]

## use the function to plot the data 
univar_data<-sampcompR::uni_compare(dfs = c("north","white"),
                                    benchmarks = c("card","card"),
                                    variables= c("age","educ","fatheduc","motheduc","wage","IQ"),
                                    funct = "abs_rel_mean",
                                    nboots=0,
                                    summetric="rmse2",
                                    data=TRUE)
#> Error in get(dfs[i]): object 'north' not found

table<-sampcompR::uni_compare_table(univar_data)
#> Error: object 'univar_data' not found
noquote(table)
#> function (..., exclude = if (useNA == "no") c(NA, NaN), useNA = c("no", 
#>     "ifany", "always"), dnn = list.names(...), deparse.level = 1) 
#> {
#>     list.names <- function(...) {
#>         l <- as.list(substitute(list(...)))[-1L]
#>         if (length(l) == 1L && is.list(..1) && !is.null(nm <- names(..1))) 
#>             return(nm)
#>         nm <- names(l)
#>         fixup <- if (is.null(nm)) 
#>             seq_along(l)
#>         else nm == ""
#>         dep <- vapply(l[fixup], function(x) switch(deparse.level + 
#>             1, "", if (is.symbol(x)) as.character(x) else "", 
#>             deparse(x, nlines = 1)[1L]), "")
#>         if (is.null(nm)) 
#>             dep
#>         else {
#>             nm[fixup] <- dep
#>             nm
#>         }
#>     }
#>     miss.use <- missing(useNA)
#>     miss.exc <- missing(exclude)
#>     useNA <- if (miss.use && !miss.exc && !match(NA, exclude, 
#>         nomatch = 0L)) 
#>         "ifany"
#>     else match.arg(useNA)
#>     doNA <- useNA != "no"
#>     if (!miss.use && !miss.exc && doNA && match(NA, exclude, 
#>         nomatch = 0L)) 
#>         warning("'exclude' containing NA and 'useNA' != \"no\"' are a bit contradicting")
#>     args <- list(...)
#>     if (length(args) == 1L && is.list(args[[1L]])) {
#>         args <- args[[1L]]
#>         if (length(dnn) != length(args)) 
#>             dnn <- paste(dnn[1L], seq_along(args), sep = ".")
#>     }
#>     if (!length(args)) 
#>         stop("nothing to tabulate")
#>     bin <- 0L
#>     lens <- NULL
#>     dims <- integer()
#>     pd <- 1L
#>     dn <- NULL
#>     for (a in args) {
#>         if (is.null(lens)) 
#>             lens <- length(a)
#>         else if (length(a) != lens) 
#>             stop("all arguments must have the same length")
#>         fact.a <- is.factor(a)
#>         if (doNA) 
#>             aNA <- anyNA(a)
#>         if (!fact.a) {
#>             a0 <- a
#>             op <- options(warn = 2)
#>             on.exit(options(op))
#>             a <- factor(a, exclude = exclude)
#>             options(op)
#>         }
#>         add.na <- doNA
#>         if (add.na) {
#>             ifany <- (useNA == "ifany")
#>             anNAc <- anyNA(a)
#>             add.na <- if (!ifany || anNAc) {
#>                 ll <- levels(a)
#>                 if (add.ll <- !anyNA(ll)) {
#>                   ll <- c(ll, NA)
#>                   TRUE
#>                 }
#>                 else if (!ifany && !anNAc) 
#>                   FALSE
#>                 else TRUE
#>             }
#>             else FALSE
#>         }
#>         if (add.na) 
#>             a <- factor(a, levels = ll, exclude = NULL)
#>         else ll <- levels(a)
#>         a <- as.integer(a)
#>         if (fact.a && !miss.exc) {
#>             ll <- ll[keep <- which(match(ll, exclude, nomatch = 0L) == 
#>                 0L)]
#>             a <- match(a, keep)
#>         }
#>         else if (!fact.a && add.na) {
#>             if (ifany && !aNA && add.ll) {
#>                 ll <- ll[!is.na(ll)]
#>                 is.na(a) <- match(a0, c(exclude, NA), nomatch = 0L) > 
#>                   0L
#>             }
#>             else {
#>                 is.na(a) <- match(a0, exclude, nomatch = 0L) > 
#>                   0L
#>             }
#>         }
#>         nl <- length(ll)
#>         dims <- c(dims, nl)
#>         if (prod(dims) > .Machine$integer.max) 
#>             stop("attempt to make a table with >= 2^31 elements")
#>         dn <- c(dn, list(ll))
#>         bin <- bin + pd * (a - 1L)
#>         pd <- pd * nl
#>     }
#>     names(dn) <- dnn
#>     bin <- bin[!is.na(bin)]
#>     if (length(bin)) 
#>         bin <- bin + 1L
#>     y <- array(tabulate(bin, pd), dims, dimnames = dn)
#>     class(y) <- "table"
#>     y
#> }
#> <bytecode: 0x55ac628e33d0>
#> <environment: namespace:base>
```
