test_that("univariate comparison table: north/south weigthed, black/white not, rel_mean", {


   ### Test final results against the test_table
  expect_equal(uni_compare_table(local_test_data_uni1()),
               matrix(c(
                 "age", "0", "-0.012",
                 "", "(-0.004, 0.004)", "(-0.024, -0.005)",
                 "educ", "0.058", "-0.125",
                 "", "(0.04, 0.064)", "(-0.141, -0.114)",
                 "Average Error", "0.029", "0.068",
                 "RANK", "1", "2",
                 "N", "1795", "703"
               ), nrow = 7, ncol = 3, byrow = TRUE, dimnames = list(NULL, c("variables", "north", "black")))
  )
})




test_that("univariate comparison table: north/south weigthed, black/white not, d_mean", {
  
  
  ### Test final results against the test_table
  expect_equal(uni_compare_table(local_test_data_uni2()),
               matrix(c(
                 "age", "0.156", "-0.342",
                 "", "(0.022, 0.259)", "(-0.69, -0.131)",
                 "educ", "1.099", "-1.704",
                 "", "(0.871, 1.192)", "(-1.932, -1.558)",
                 "RMSE", "0.785", "1.229",
                 "RANK", "1", "2",
                 "N", "1795", "703"
               ), nrow = 7, ncol = 3, byrow = TRUE, dimnames = list(NULL, c("variables", "north", "black")))
               
  )
})



test_that("univariate comparison table: north/south weigthed, black/white not, analytic CIs", {
  
  
  ### Test final results against the test_table
  expect_equal(uni_compare_table(local_test_data_uni1(summet="rmse1",nboots = 0, funct= "rel_mean")),
               matrix(c("age", "0", "-0.012",
                        "", "(-0.005, 0.005)", "(-0.02, -0.004)",
                        "educ", "0.058", "-0.125",
                        "", "(0.05, 0.067)", "(-0.139, -0.111)",
                        "RANK", "1", "2",
                        "N", "1795", "703"),
                      nrow = 6, ncol = 3, byrow = TRUE,
                      dimnames = list(NULL, c("variables", "north", "black")))
  )
  
  expect_equal(uni_compare_table(local_test_data_uni1(summet="rmse1",nboots = 0, funct= "abs_rel_mean")),
               matrix(c("age", "0", "0.012",
                        "", "(-0.005, 0.005)", "(0.004, 0.02)",
                        "educ", "0.058", "0.125",
                        "", "(0.05, 0.067)", "(0.111, 0.139)",
                        "RANK", "1", "2",
                        "N", "1795", "703"),
                      nrow = 6, ncol = 3, byrow = TRUE, 
                      dimnames = list(NULL, c("variables", "north", "black")))
  
  )
  
  expect_equal(uni_compare_table(local_test_data_uni1(summet="rmse1",nboots = 0, funct= "d_mean")),
               matrix_data <- matrix(c("age", "0.008", "-0.342",
                                       "", "(-0.135, 0.151)", "(-0.568, -0.115)",
                                       "educ", "0.764", "-1.704",
                                       "", "(0.65, 0.878)", "(-1.898, -1.51)",
                                       "RANK", "1", "2",
                                       "N", "1795", "703"),
                                     nrow = 6, ncol = 3, byrow = TRUE,
                                     dimnames = list(NULL, c("variables", "north", "black")))
               
  )
  
  expect_equal(uni_compare_table(local_test_data_uni1(summet="rmse1",nboots = 0, funct= "prop_modecat")),
               matrix_data <- matrix(c("age", "-0.004", "0.007",
                                       "", "(-0.018, 0.011)", "(-0.018, 0.032)",
                                       "educ", "0.037", "0.03",
                                       "", "(0.015, 0.059)", "(-0.005, 0.066)",
                                       "RANK", "2", "1",
                                       "N", "1795", "703"),
                                     nrow = 6, ncol = 3, byrow = TRUE,
                                     dimnames = list(NULL, c("variables", "north", "black"))))
  
  expect_equal(uni_compare_table(local_test_data_uni1(summet="rmse1",nboots = 0, funct= "d_median")),
               matrix_data <- matrix(c("age", "", "educ", "", "RANK", "N",
                                       "0", "(-0.143, 0.143)", "0", "(-0.114, 0.114)", "1", "1795",
                                       "-1", "(-1.226, -0.774)", "-1", "(-1.194, -0.806)", "2", "703"),
                                     nrow = 6, ncol = 3, byrow = FALSE,
                                     dimnames = list(NULL, c("variables", "north", "black"))))
  
})



test_that("Test R_Indicator", {
  expect_equal(local_test_data_uni3(),
               list(
                 card1 = c("R-Indicator" = 0.8610737759, "SE" = 0.0001926046),
                 card2 = c("R-Indicator" = 0.715819853, "SE" = 0.001614479)
               ))
  
})
  
 
                  