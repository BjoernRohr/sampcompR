test_that("Does multi_compare produce the same results als multi_compare 2? :UnWeighted", {
  expect_equal(local_test_multi_compare1_1()[2], 
               local_test_multi_compare2_1()[2])
  
  expect_equal(local_test_multi_compare1_1()[3], 
               local_test_multi_compare2_1()[3])
  
  expect_equal(local_test_multi_compare1_1()[4], 
               local_test_multi_compare2_1()[4])
  
  expect_equal(local_test_multi_compare1_1()[5], 
               local_test_multi_compare2_1()[5])
  
  expect_equal(local_test_multi_compare1_1()[6], 
               local_test_multi_compare2_1()[6])
  
  expect_equal(local_test_multi_compare1_1()[7], 
               local_test_multi_compare2_1()[7])
  
  expect_equal(local_test_multi_compare1_1()[8], 
               local_test_multi_compare2_1()[8])
  
  expect_equal(local_test_multi_compare1_1()[9], 
               local_test_multi_compare2_1()[9])
  
  expect_equal(local_test_multi_compare1_1()[10], 
               local_test_multi_compare2_1()[10])
  
  expect_equal(local_test_multi_compare1_1()[11], 
               local_test_multi_compare2_1()[11])
  
  expect_equal(local_test_multi_compare1_1()[12], 
               local_test_multi_compare2_1()[12])
  
  expect_equal(local_test_multi_compare1_1()[13], 
               local_test_multi_compare2_1()[13])
  
  expect_equal(local_test_multi_compare1_1()[14], 
               local_test_multi_compare2_1()[14])
  
  expect_equal(local_test_multi_compare1_1()[15], 
               local_test_multi_compare2_1()[15])
  
  expect_equal(local_test_multi_compare1_1()[16], 
               local_test_multi_compare2_1()[16])
  
  expect_equal(local_test_multi_compare1_1()[17], 
               local_test_multi_compare2_1()[17])
  
  expect_equal(local_test_multi_compare1_1()[18], 
               local_test_multi_compare2_1()[18])
  
  expect_equal(local_test_multi_compare1_1()[19], 
               local_test_multi_compare2_1()[19])
  
  expect_equal(local_test_multi_compare1_1()[20], 
               local_test_multi_compare2_1()[20])
  
  expect_equal(local_test_multi_compare1_1()[21], 
               local_test_multi_compare2_1()[21])
  
  expect_equal(local_test_multi_compare1_1()[22], 
               local_test_multi_compare2_1()[22])
})




test_that("Does multi_compare produce the same results als multi_compare 2? :Weighted", {
  expect_equal(local_test_multi_compare1_2()[2:22], 
               local_test_multi_compare2_2()[2:22])
  
  })


test_that("Does multi_compare2 produce the same results as glm?", {
  expect_equal(round(as.vector(local_test_multi_compare2_1()$coefs_data1[,1]),3), 
               normal_glm())
  
})
