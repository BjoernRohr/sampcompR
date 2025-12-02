# Returns a table based on the information of a `multi_compare_object` that indicates the proportion of biased variables. It can be outputted as HTML or LaTex Table, for example with the help of the [stargazer](https://rdrr.io/pkg/stargazer/man/stargazer.html) function.

Returns a table based on the information of a `multi_compare_object`
that indicates the proportion of biased variables. It can be outputted
as HTML or LaTex Table, for example with the help of the
[stargazer](https://rdrr.io/pkg/stargazer/man/stargazer.html) function.

## Usage

``` r
multi_per_variable(
  multi_compare_objects,
  type = "coefs",
  label_df = NULL,
  lables_coefs = NULL,
  lables_models = NULL,
  ndigits = 1
)
```

## Arguments

- multi_compare_objects:

  A object returned by the
  [`multi_compare`](https://bjoernrohr.github.io/sampcompR/reference/multi_compare.md)
  function. Object can either be inserted as single object or as a
  character string containing the names of multiple objects.

- type:

  The `type` of table, can either be `"coefs"`, `"models"`, or
  `"complete"`. When coefs is chosen, the average bias of the
  coefficients is outputted, when models is chosen, the average bias for
  the models is outputted, and when complete is chosen, both are
  outputted.

- label_df:

  A character vector containing labels for the data frames.

- lables_coefs:

  A character vector containing labels for the coefficients.

- lables_models:

  A character vector containing labels for the models.

- ndigits:

  Number of digits that is shown in the table.

## Value

A matrix, that indicates the proportion of bias for every individual
coefficient or model for multivariate comparisons. This is given
separately for every comparison, as well as averaged over comparisons.

## Examples

``` r
data("card")

north <- card[card$south==0,]
white <- card[card$black==0,]

## use the function to plot the data
multi_data1 <- sampcompR::multi_compare(df = north, 
                                        bench = card,
                                        independent = c("age","fatheduc","motheduc","IQ"),
                                        dependent = c("educ","wage"),
                                        family = "ols") 
#> 
#> Difference in coeficients between sets of respondents 
#>  
#>          educ         wage        
#> age      -2.43e-02    -8.55e-01   
#> fatheduc -2.37e-02    -2.93e-01   
#> motheduc 1.23e-02     2.35e+00    
#> IQ       -7.25e-03    4.80e-01    
#> 
#> Overall difference between north & card: 0% of coeficients are significant different
#> (*p<0.05 ; **p<0.005 ; ***p<0.001;  for t-test robust standard errors are used) 
#>  

multi_data2 <- sampcompR::multi_compare(df = white, 
                                        bench = card,
                                        independent = c("age","fatheduc","motheduc","IQ"),
                                        dependent = c("educ","wage"),
                                        family = "ols") 
#> 
#> Difference in coeficients between sets of respondents 
#>  
#>          educ         wage        
#> age      -1.20e-02    -9.51e-01   
#> fatheduc -1.45e-02    1.28e-01    
#> motheduc 9.99e-03     2.03e-01    
#> IQ       -9.28e-03    3.61e-01    
#> 
#> Overall difference between white & card: 0% of coeficients are significant different
#> (*p<0.05 ; **p<0.005 ; ***p<0.001;  for t-test robust standard errors are used) 
#>  

table<-sampcompR::multi_per_variable(multi_compare_objects = c("multi_data1","multi_data2"))
#> Error in purrr::map(multi_compare_objects, ~multi_same_func(multi_compare_object = .x,     p_adjust = p_adjust)): ℹ In index: 1.
#> Caused by error in `get()`:
#> ! object 'multi_data1' not found
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
#> <bytecode: 0x55cb619963d0>
#> <environment: namespace:base>
```
