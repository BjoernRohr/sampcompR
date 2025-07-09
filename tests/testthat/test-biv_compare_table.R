
##############
### Test 1 ###
##############


test_that("biv_compare_table, type=diff, weighted df", {
  expect_equal(local_test_data_biv1(tableargs=list(type="diff")), matrix(c(
    "", "", "", "",
    " 0.11*  ", "", "", "",
    " 0.05   ", "-0.05   ", "", "",
    " 0.05   ", "-0.01   ", "-0.02   ", ""
  ), nrow = 4, ncol = 4, byrow = TRUE, dimnames = list(c("age", "educ", "wage", "IQ"), c("age", "educ", "wage", "IQ"))))
})

##############
### Test 2 ###
##############

test_that("biv_compare_table, type=dfs, weighted data", {
  expect_equal(local_test_data_biv1(tableargs=list(type="dfs")), matrix(c(
    "", "", "", "",
    " 0.06*  ", "", "", "",
    " 0.35***", " 0.19***", "", "",
    "-0.05   ", " 0.50***", " 0.13***", ""
  ), nrow = 4, ncol = 4, byrow = TRUE, dimnames = list(c("age", "educ", "wage", "IQ"), c("age", "educ", "wage", "IQ")))
  )
})

##############
### Test 3 ###
##############

test_that("biv_compare_table, type=benchmarks, weighted data", {
  expect_equal(local_test_data_biv1(tableargs=list(type="benchmarks")), matrix(c(
    "", "", "", "",
    "-0.05   ", "", "", "",
    " 0.30***", " 0.24***", "", "",
    "-0.10** ", " 0.52***", " 0.15***", ""
  ), nrow = 4, ncol = 4, byrow = TRUE, dimnames = list(c("age", "educ", "wage", "IQ"), c("age", "educ", "wage", "IQ")))
  )
})

##############
### Test 4 ###
##############

test_that("biv_compare_table, type=diff, un_weighted data", {
  expect_equal(local_test_data_biv1(tableargs=list(type="diff",
                                                   comparison_number = 2)), 
               matrix(c(
                 "",         "",        "",     "",
                 "-0.09   ", "",        "",     "",
                 "-0.14*  ", " 0.07   ", "",     "",
                 " 0.02   ",     "-0.12*  ",  " 0.11   ", ""
               ), nrow = 4, ncol = 4, byrow = TRUE, dimnames = list(c("age", "educ", "wage", "IQ"), c("age", "educ", "wage", "IQ")))
               
  )
})


##############
### Test 5 ###
##############

test_that("biv_compare_table, type=diff, un_weighted data, bonferroni", {
  expect_equal(local_test_data_biv2(tableargs=list(type="diff",
                                                   comparison_number = 1)), 
               table_matrix <- matrix(c(
                 "",       "",         "",      "",
                 " 0.12** ",  "",         "",      "",
                 " 0.12** ", "-0.18***", "",      "",
                 " 0.04   ",   " 0.00   ",        "-0.07   ", ""
               ), nrow = 4, ncol = 4, byrow = TRUE, dimnames = list(c("age", "educ", "wage", "IQ"), c("age", "educ", "wage", "IQ")))
               
  )
})






##############
### Test 6 ###
##############

test_that("biv_compare_table, type=diff, weighted df, pairwise", {
  expect_equal(local_test_data_biv1(remove_nas="pairwise",tableargs=list(type="diff")), matrix(c(
    "", "", "", "",
    " 0.12** ", "", "", "",
    " 0.09** ", "-0.15***", "", "",
    " 0.05   ", "-0.01   ", "-0.02   ", ""
  ), nrow = 4, ncol = 4, byrow = TRUE, dimnames = list(c("age", "educ", "wage", "IQ"), c("age", "educ", "wage", "IQ"))))
})

##############
### Test 7 ###
##############

test_that("biv_compare_table, type=dfs, weighted data, pairwise", {
  expect_equal(local_test_data_biv1(remove_nas="pairwise",
                                    tableargs=list(type="dfs")), matrix(c(
    "", "", "", "",
    " 0.04   ", "", "", "",
    " 0.35***", " 0.19***", "", "",
    "-0.05   ", " 0.50***", " 0.13***", ""
  ), nrow = 4, ncol = 4, byrow = TRUE, dimnames = list(c("age", "educ", "wage", "IQ"), c("age", "educ", "wage", "IQ")))
  )
})

##############
### Test 8 ###
##############

test_that("biv_compare_table, type=benchmarks, weighted data, pairwise", {
  expect_equal(local_test_data_biv1(remove_nas="pairwise",
                                    tableargs=list(type="benchmarks")), matrix(c(
    "", "", "", "",
    "-0.08** ", "", "", "",
    " 0.26***", " 0.33***", "", "",
    "-0.10** ", " 0.52***", " 0.15***", ""
  ), nrow = 4, ncol = 4, byrow = TRUE, dimnames = list(c("age", "educ", "wage", "IQ"), c("age", "educ", "wage", "IQ")))
  )
})

##############
### Test 9 ###
##############

test_that("biv_compare_table, type=diff, un_weighted data, pairwise", {
  expect_equal(local_test_data_biv1(remove_nas="pairwise",
                                    tableargs=list(type="diff",
                                                   comparison_number = 2)), 
               matrix(c(
                 "",         "",        "",     "",
                 "-0.13** ", "",        "",     "",
                 "-0.21***", " 0.14***", "",     "",
                 " 0.02   ",     "-0.12*  ",  " 0.11   ", ""
               ), nrow = 4, ncol = 4, byrow = TRUE, dimnames = list(c("age", "educ", "wage", "IQ"), c("age", "educ", "wage", "IQ")))
               
  )
})





