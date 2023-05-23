
##############
### Test 1 ###
##############

test_that("biv_compare_table, type=diff, weighted df", {
  expect_equal(biv_compare_table(local_test_data_biv1(),type="diff"), matrix(c(
    "", "", "", "",
    "0.11**", "", "", "",
    "0.05", "-0.05", "", "",
    "0.05", "-0.01", "-0.02", ""
  ), nrow = 4, ncol = 4, byrow = TRUE, dimnames = list(c("age", "educ", "wage", "IQ"), c("age", "educ", "wage", "IQ"))))
})

##############
### Test 2 ###
##############

test_that("biv_compare_table, type=dfs, weighted data", {
  expect_equal(biv_compare_table(local_test_data_biv1(),type="dfs"), matrix(c(
    "", "", "", "",
    "0.06", "", "", "",
    "0.35***", "0.19***", "", "",
    "-0.05*", "0.5***", "0.13***", ""
  ), nrow = 4, ncol = 4, byrow = TRUE, dimnames = list(c("age", "educ", "wage", "IQ"), c("age", "educ", "wage", "IQ")))
  )
})

##############
### Test 3 ###
##############

test_that("biv_compare_table, type=benchmarks, weighted data", {
  expect_equal(biv_compare_table(local_test_data_biv1(),type="benchmarks"), matrix(c(
    "", "", "", "",
    "-0.05**", "", "", "",
    "0.3***", "0.24***", "", "",
    "-0.1**", "0.52***", "0.15***", ""
  ), nrow = 4, ncol = 4, byrow = TRUE, dimnames = list(c("age", "educ", "wage", "IQ"), c("age", "educ", "wage", "IQ")))
  )
})

##############
### Test 4 ###
##############

test_that("biv_compare_table, type=diff, un_weighted data", {
  expect_equal(biv_compare_table(local_test_data_biv1(),type="diff", 
                                 comparison_number = 2), 
               matrix(c(
                 "",         "",        "",     "",
                 "-0.15***", "",        "",     "",
                 "-0.22***", "0.17***", "",     "",
                 "0.01",     "-0.12*",  "0.12", ""
               ), nrow = 4, ncol = 4, byrow = TRUE, dimnames = list(c("age", "educ", "wage", "IQ"), c("age", "educ", "wage", "IQ")))
               
  )
})


##############
### Test 5 ###
##############

test_that("biv_compare_table, type=diff, un_weighted data, bonferroni", {
  expect_equal(biv_compare_table(local_test_data_biv2(),type="diff", 
                                 comparison_number = 1), 
               table_matrix <- matrix(c(
                 "",       "",         "",      "",
                 "0.12*",  "",         "",      "",
                 "0.12**", "-0.18***", "",      "",
                 "0.04",   "0",        "-0.07", ""
               ), nrow = 4, ncol = 4, byrow = TRUE, dimnames = list(c("age", "educ", "wage", "IQ"), c("age", "educ", "wage", "IQ")))
               
  )
})





