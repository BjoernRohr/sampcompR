test_that("multi comparison table: north/south weigthed, black/white not, rel_mean", {
  
  
  ### Test final results against the test_table
  expect_equal(local_test_multi_compare1_1(),
               matrix(
                 c(
                   "north", "age", "-0.079*  ", "-3.740   ",
                   "", "", "(0.034)", "(4.455)",
                   "", "fatheduc", "-0.044   ", "-3.110   ",
                   "", "", "(0.035)", "(4.582)",
                   "", "motheduc", "  0.043   ", "  5.400   ",
                   "", "", "(0.041)", "(5.376)",
                   "", "IQ", "-0.019** ", "  0.834   ",
                   "", "", "(0.007)", "(0.911)"
                 ),
                 nrow = 8,      # Number of rows
                 ncol = 4,      # Number of columns
                 byrow = TRUE,   # Fill matrix by rows
                 dimnames = list(NULL, c("data_frames","variables", "educ", "wage")))
               )
})


test_that("Does multi_compare produce the same results als multi_compare 2? :UnWeighted", {
  expect_equal(local_test_multi_compare1_1_all()[2],
               local_test_multi_compare2_1_all()[2])

  expect_equal(local_test_multi_compare1_1_all()[3],
               local_test_multi_compare2_1_all()[3])

  expect_equal(local_test_multi_compare1_1_all()[4],
               local_test_multi_compare2_1_all()[4])

  expect_equal(local_test_multi_compare1_1_all()[5],
               local_test_multi_compare2_1_all()[5])

  expect_equal(local_test_multi_compare1_1_all()[6],
               local_test_multi_compare2_1_all()[6])

  expect_equal(local_test_multi_compare1_1_all()[7],
               local_test_multi_compare2_1_all()[7])

  expect_equal(local_test_multi_compare1_1_all()[8],
               local_test_multi_compare2_1_all()[8])

  expect_equal(local_test_multi_compare1_1_all()[9],
               local_test_multi_compare2_1_all()[9])

  expect_equal(local_test_multi_compare1_1_all()[10],
               local_test_multi_compare2_1_all()[10])

  expect_equal(local_test_multi_compare1_1_all()[11],
               local_test_multi_compare2_1_all()[11])

  expect_equal(local_test_multi_compare1_1_all()[12],
               local_test_multi_compare2_1_all()[12])

  expect_equal(local_test_multi_compare1_1_all()[13],
               local_test_multi_compare2_1_all()[13])

  expect_equal(local_test_multi_compare1_1_all()[14],
               local_test_multi_compare2_1_all()[14])

  expect_equal(local_test_multi_compare1_1_all()[15],
               local_test_multi_compare2_1_all()[15])

  expect_equal(local_test_multi_compare1_1_all()[16],
               local_test_multi_compare2_1_all()[16])

  expect_equal(local_test_multi_compare1_1_all()[17],
               local_test_multi_compare2_1_all()[17])

  expect_equal(local_test_multi_compare1_1_all()[18],
               local_test_multi_compare2_1_all()[18])

  expect_equal(local_test_multi_compare1_1_all()[19],
               local_test_multi_compare2_1_all()[19])

  expect_equal(local_test_multi_compare1_1_all()[20],
               local_test_multi_compare2_1_all()[20])

  expect_equal(local_test_multi_compare1_1_all()[21],
               local_test_multi_compare2_1_all()[21])

  expect_equal(local_test_multi_compare1_1_all()[22],
               local_test_multi_compare2_1_all()[22])
})




test_that("Does multi_compare produce the same results als multi_compare 2? :Weighted", {
  expect_equal(local_test_multi_compare1_2_all()[2:22],
               local_test_multi_compare2_2_all()[2:22])

  })


test_that("Does multi_compare2 produce the same results as glm?", {
  expect_equal(round(as.vector(local_test_multi_compare2_1_all()$coefs_data1[,1]),3),
               normal_glm())

})
