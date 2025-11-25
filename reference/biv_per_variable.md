# Returns a table based on the information of a `biv_compare_object` that indicates the proportion of biased variables. It can be outputted as HTML or LaTex Table, for example with the help of the [stargazer](https://rdrr.io/pkg/stargazer/man/stargazer.html) function.

Returns a table based on the information of a `biv_compare_object` that
indicates the proportion of biased variables. It can be outputted as
HTML or LaTex Table, for example with the help of the
[stargazer](https://rdrr.io/pkg/stargazer/man/stargazer.html) function.

## Usage

``` r
biv_per_variable(
  biv_compare_object,
  ndigits = 1,
  varlabels = NULL,
  label_df = NULL
)
```

## Arguments

- biv_compare_object:

  A object returned by the
  [`biv_compare`](https://bjoernrohr.github.io/sampcompR/reference/biv_compare.md)
  function.

- ndigits:

  Number of digits that is shown in the table.

- varlabels:

  A character vector containing labels for the variables.

- label_df:

  A character vector containing labels for the data frames.

## Value

A matrix, that indicates the proportion of bias for every individual
variable. This is given separately for every comparison, as well as
averaged over comparisons.

## Examples

``` r
data("card")

north <- card[card$south==0,]
white <- card[card$black==0,]

## use the function to plot the data 
bivar_data<-sampcompR::biv_compare(dfs = c("north","white"),
                                   benchmarks = c("card","card"),
                                   variables= c("age","educ","fatheduc","motheduc","wage","IQ"),
                                   data=TRUE)
#> Error in get(dfs[i]): object 'north' not found

table<-sampcompR::biv_per_variable(bivar_data)
#> Error: object 'bivar_data' not found
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
