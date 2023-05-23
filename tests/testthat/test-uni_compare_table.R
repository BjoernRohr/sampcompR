test_that("univariate comparison table: north/south weigthed, black/white not, rel_mean", {


   ### Test final results against the test_table
  expect_equal(uni_compare_table(local_test_data_uni1()),
               matrix(c(
                 "age", "0", "-0.012",
                 "", "(-0.004, 0.007)", "(-0.024, -0.005)",
                 "educ", "0.058", "-0.125",
                 "", "(0.051, 0.07)", "(-0.141, -0.114)",
                 "Average Error", "0.029", "0.068",
                 "RANK", "1", "2",
                 "N", "1796", "703"
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


