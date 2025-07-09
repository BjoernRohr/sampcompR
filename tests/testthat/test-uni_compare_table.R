test_that("univariate comparison table: north/south weigthed, black/white not, rel_mean", {


   ### Test final results against the test_table
  expect_equal(sampcompR::uni_compare_table(local_test_data_uni1()),
               matrix(c(
                 "age", "0.000", "-0.012",
                 "", "(-0.006, 0.006)", "(-0.020, -0.004)",
                 "educ", "0.058", "-0.125",
                 "", "( 0.047, 0.070)", "(-0.140, -0.110)",
                 "Average Error", "0.029", "0.068",
                 "RANK", "1", "2",
                 "N", "1795", "703"
               ), nrow = 7, ncol = 3, byrow = TRUE, dimnames = list(NULL, c("Variables", "north", "black")))
  )
})


# if(Sys.info()[1]!="Darwin"| is.na(Sys.getenv("GITHUB_ACTIONS",unset = NA))){
# test_that("univariate comparison table: north/south weigthed, black/white not, d_mean", {
#   
#   
#   ### Test final results against the test_table
#   expect_equal(uni_compare_table(local_test_data_uni2()),
#                matrix(c(
#                  "age", "0.156", "-0.342",
#                  "", "(-0.023, 0.336)", "(-0.577, -0.106)",
#                  "educ", "1.099", "-1.704",
#                  "", "( 0.949, 1.248)", "(-1.908, -1.500)",
#                  "RMSE", "0.785", "1.229",
#                  "RANK", "1", "2",
#                  "N", "1795", "703"
#                ), nrow = 7, ncol = 3, byrow = TRUE, dimnames = list(NULL, c("Variables", "north", "black")))
#                
#   )
#  })}



# if(Sys.info()[1]=="Darwin" & !is.na(Sys.getenv("GITHUB_ACTIONS",unset = NA))){
#   test_that("univariate comparison table: north/south weigthed, black/white not, d_mean", {
# 
# 
#     ### Test final results against the test_table
#     expect_equal(uni_compare_table(local_test_data_uni2()),
#                  matrix(c(
#                    "age", "0.156", "-0.342",
#                    "", "(-0.021, 0.333)", "(-0.576, -0.108)",
#                    "educ", "1.099", "-1.704",
#                    "", "( 0.949, 1.248)", "(-1.908, -1.500)",
#                    "RMSE", "0.785", "1.229",
#                    "RANK", "1", "2",
#                    "N", "1795", "703"
#                  ), nrow = 7, ncol = 3, byrow = TRUE, dimnames = list(NULL, c("Variables", "north", "black")))
# 
#     )
#   })}

test_that("univariate comparison table: north/south weigthed, black/white not, d_mean", {
  
  
  ### Test final results against the test_table
  expect_equal(sampcompR::uni_compare_table(local_test_data_uni1(summet="rmse1",nboots = 0, funct= "d_mean")),
               matrix(c("age", "0.008", "-0.342",
                        "", "(-0.135, 0.151)", "(-0.568, -0.115)",
                        "educ", "0.764", "-1.704",
                        "", "( 0.650, 0.878)", "(-1.898, -1.510)",
                        "RMSE","0.541","1.229",
                        "RANK", "1", "2",
                        "N", "1795", "703"),
                      nrow = 7, ncol = 3, byrow = TRUE,
                      dimnames = list(NULL, c("Variables", "north", "black")))
  )
})

test_that("univariate comparison table: north/south weigthed, black/white not, analytic CIs", {
  
  
  ### Test final results against the test_table
  expect_equal(sampcompR::uni_compare_table(local_test_data_uni1(summet="rmse1",nboots = 0, funct= "rel_mean")),
               matrix(c("age", "0.000", "-0.012",
                        "", "(-0.005, 0.005)", "(-0.020, -0.004)",
                        "educ", "0.058", "-0.125",
                        "", "( 0.050, 0.067)", "(-0.139, -0.111)",
                        "RMSE","0.041","0.089",
                        "RANK", "1", "2",
                        "N", "1795", "703"),
                      nrow = 7, ncol = 3, byrow = TRUE,
                      dimnames = list(NULL, c("Variables", "north", "black")))
  )
  
  expect_equal(sampcompR::uni_compare_table(local_test_data_uni1(summet="rmse1",nboots = 0, funct= "abs_rel_mean")),
               matrix(c("age", "0.000", "0.012",
                        "", "(-0.005, 0.005)", "(0.004, 0.020)",
                        "educ", "0.058", "0.125",
                        "", "( 0.050, 0.067)", "(0.111, 0.139)",
                        "RMSE","0.041","0.089",
                        "RANK", "1", "2",
                        "N", "1795", "703"),
                      nrow = 7, ncol = 3, byrow = TRUE, 
                      dimnames = list(NULL, c("Variables", "north", "black")))
  
  )
  
  expect_equal(as.matrix(sampcompR::uni_compare_table(local_test_data_uni1(summet="rmse1",nboots = 0, funct= "d_mean"))),
               matrix_data <- matrix(c("age", "0.008", "-0.342",
                                       "", "(-0.135, 0.151)", "(-0.568, -0.115)",
                                       "educ", "0.764", "-1.704",
                                       "", "( 0.650, 0.878)", "(-1.898, -1.510)",
                                       "RMSE","0.541","1.229",
                                       "RANK", "1", "2",
                                       "N", "1795", "703"),
                                     nrow = 7, ncol = 3, byrow = TRUE,
                                     dimnames = list(NULL, c("Variables", "north", "black")))
               
  )
  
  # expect_equal(as.matrix(uni_compare_table(local_test_data_uni1(summet="rmse1",nboots = 0, funct= "prop_modecat"))),
  #              matrix_data <- matrix(c("age", "-0.004", "0.007",
  #                                      "", "(-0.018, 0.011)", "(-0.018, 0.032)",
  #                                      "educ", " 0.037", "0.030",
  #                                      "", "( 0.015, 0.059)", "(-0.005, 0.066)",
  #                                      "RANK", "2", "1",
  #                                      "N", "1795", "703"),
  #                                    nrow = 6, ncol = 3, byrow = TRUE,
  #                                    dimnames = list(NULL, c("Variables", "north", "black"))))
  
#   expect_equal(as.matrix(uni_compare_table(local_test_data_uni1(summet="rmse1",nboots = 0, funct= "d_median"))),
#                matrix_data <- matrix(c("age", "", "educ", "", "RANK", "N",
#                                        "0.000", "(-0.143, 0.143)", "0.000", "(-0.114, 0.114)", "1", "1795",
#                                        "-1.000", "(-1.226, -0.774)", "-1.000", "(-1.194, -0.806)", "2", "703"),
#                                      nrow = 6, ncol = 3, byrow = FALSE,
#                                      dimnames = list(NULL, c("Variables", "north", "black"))))
#   
 })



test_that("Test R_Indicator", {
  expect_equal(local_test_data_uni3(),
               list(
                 card1 = c("R-Indicator" = 0.8610737759, "SE" = 0.0001926046),
                 card2 = c("R-Indicator" = 0.715819853, "SE" = 0.001614479)
               ))
  
})
  
 
                  