
df <- tibble(
  scale_sub1_1 = c(1, 2, 3),
  scale_sub1_2 = c(2, 3, 2),
  scale_sub2_1 = c(3, 2, 3),
  scale_sub2_2 = c(3, 3, 3)
)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

df <- tibble(
  scale_sub1_1 = c(1, 2, 3),
  scale_sub1_2 = c(2.2, 2.8, 2),
  scale_sub2_1 = c(3, 2, 3),
  scale_sub2_2 = c(3, 3, 3)
)


df <- tibble(
  scale_sub1_1 = c(1, 2, 3),
  scale_sub1_2 = c(2.2, 2.8, 2),
  scale_sub2_1 = c(3, 2, 3),
  scale_sub2_2 = c("3", "3", "3")
)

df_error <- tibble(
  scale_sub1_1 = c(1, 2, 3),
  scale_sub1_2 = c("uncoercible", 2, 2),
  scale_sub2_1 = c(3, 2, 3),
  scale_sub2_2 = c(3, 3, 3)
)
