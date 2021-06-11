
test_that("Returns numeric type", {

  myVec <- c(1,6,3,6,8,4,5,4,2,2)
  myScaledVec <- scale(myVec)

  expect_true(is.numeric(myScaledVec))
})
