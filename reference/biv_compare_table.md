# Returns a table based on the information of a `biv_compare_object` which can be outputted as HTML or LaTex Table, for example with the help of the [stargazer](https://rdrr.io/pkg/stargazer/man/stargazer.html) function.

Returns a table based on the information of a `biv_compare_object` which
can be outputted as HTML or LaTex Table, for example with the help of
the [stargazer](https://rdrr.io/pkg/stargazer/man/stargazer.html)
function.

## Usage

``` r
biv_compare_table(
  biv_compare_object,
  type = "diff",
  comparison_number = 1,
  ndigits = 2
)
```

## Arguments

- biv_compare_object:

  A object returned by the
  [`biv_compare`](https://bjoernrohr.github.io/sampcompR/reference/biv_compare.md)
  function.

- type:

  A character string, to choose what matrix should be printed.

  - If "dfs", a correlation matrix of all variables of comparison in the
    chosen dataframe will be returned.

  - If "benchmarks", a correlation matrix of all variables of comparison
    in the chosen benchmark will be returned.

  - if "diff", a matrix indicating the difference between the chosen
    dataframe and benchmark will be returned.

- comparison_number:

  A number indicating the data of which data frame, benchmark or
  comparison should be displayed. The maximum length is equal to the
  length of the `dfs vector` that is used to generate the
  `biv_compare_object`.

- ndigits:

  Number of digits shown in the table.

## Value

A correlation matrix, or difference matrix based on information of a
`biv_compare_object`.

## Examples

``` r
## Get Data for comparison

data("card")

north <- card[card$south==0,]
white <- card[card$black==0,]

## use the function to plot the data 
bivar_data<-sampcompR::biv_compare(dfs = c("north","white"),
                                   benchmarks = c("card","card"),
                                   variables= c("age","educ","fatheduc","motheduc","wage","IQ"),
                                   data=TRUE)
#> Error in get(dfs[i]): object 'north' not found
                        
table<-sampcompR::biv_compare_table(bivar_data, type="diff", comparison_number=1)
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
#> <bytecode: 0x5646796663d0>
#> <environment: namespace:base>

```
