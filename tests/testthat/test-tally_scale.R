
test_that("Basic tally with join works", {
  df_in <- dplyr::tribble(
    ~scale_sub1_1, ~scale_sub1_2, ~scale_sub2_1, ~scale_sub2_2,
    1,             2,             3,             3,
    2,             3,             2,             3,
    3,             2,             3,             3
  )

  df_out <- dplyr::tribble(
    ~scale_sub1_1, ~scale_sub1_2, ~scale_sub2_1, ~scale_sub2_2, ~scale, ~scale_sub1, ~scale_sub2,
    1,             2,             3,             3,             9,      3,           6,
    2,             3,             2,             3,             10,     5,           5,
    3,             2,             3,             3,             11,     5,           6
  )

  df_result <- df_in %>%
    tally_scale("scale") %>%
    tally_scale("scale_sub1") %>%
    tally_scale("scale_sub2")
  expect_identical(df_result, df_out)
})

test_that("Basic tally with join works with NAs", {
  df_in <- dplyr::tribble(
    ~scale_sub1_1, ~scale_sub1_2, ~scale_sub2_1, ~scale_sub2_2,
    NA,             2,             3,             3,
    2,             3,             2,             3,
    3,             2,             3,             3
  )

  df_out <- dplyr::tribble(
    ~scale_sub1_1, ~scale_sub1_2, ~scale_sub2_1, ~scale_sub2_2, ~scale, ~scale_sub1, ~scale_sub2,
    NA,            2,             3,             3,             NA,     NA,         6,
    2,             3,             2,             3,             10,     5,          5,
    3,             2,             3,             3,             11,     5,          6
  )

  df_result <- df_in %>%
    tally_scale("scale") %>%
    tally_scale("scale_sub1") %>%
    tally_scale("scale_sub2")
  expect_identical(df_result, df_out)
})




test_that("Basic tally with join and rename works", {

  df_in <- dplyr::tribble(
    ~scale_sub1_1, ~scale_sub1_2, ~scale_sub2_1, ~scale_sub2_2,
    1,             2,             3,             3,
    2,             3,             2,             3,
    3,             2,             3,             3
  )
  df_out <- dplyr::tribble(
    ~scale_sub1_1, ~scale_sub1_2, ~scale_sub2_1, ~scale_sub2_2, ~scale_total, ~scale_sub1_total, ~scale_sub2_total,
    1,             2,             3,             3,             9,            3,                 6,
    2,             3,             2,             3,             10,           5,                 5,
    3,             2,             3,             3,             11,           5,                 6
  )

  df_result <- df_in %>%
    tally_scale("scale", "scale_total") %>%
    tally_scale("scale_sub1", "scale_sub1_total") %>%
    tally_scale("scale_sub2", "scale_sub2_total")
  expect_identical(df_result, df_out)
})

df_in <- dplyr::tribble(
  ~scale_sub1_1, ~scale_sub1_2, ~scale_sub2_1, ~scale_sub2_2,
  1,             2,             3,             3,
  2,             3,             2,             3,
  3,             2,             3,             3
)
df_out <- dplyr::tribble(
  ~scale,
  9,
  10,
  11
)

test_that("Basic tally without join works", {
  df_result <- df_in %>%
    tally_scale("scale", join_function = NULL)
  expect_identical(df_result, df_out)
})

df_in <- dplyr::tribble(
  ~scale_sub1_1, ~scale_sub1_2, ~scale_sub2_1, ~scale_sub2_2,
  0.8,           2.2,           3,             3,
  2,             3,             2,             3,
  3,             2,             3,             3
)
df_out <- dplyr::tribble(
  ~scale_sub1_1, ~scale_sub1_2, ~scale_sub2_1, ~scale_sub2_2, ~scale, ~scale_sub1, ~scale_sub2,
  .8,            2.2,           3,             3,             9,      3,           6,
  2,             3,             2,             3,             10,     5,           5,
  3,             2,             3,             3,             11,     5,           6
)


test_that("Tally with decimals works", {
  df_result <- df_in %>%
    tally_scale("scale") %>%
    tally_scale("scale_sub1") %>%
    tally_scale("scale_sub2")
  expect_identical(df_result, df_out)
})

df_in <- dplyr::tribble(
  ~scale_sub1_1, ~scale_sub1_2, ~scale_sub2_1, ~scale_sub2_2,
  "1",           "2",             "3",             "3",
  "2",           "3",             "2",             "3",
  "3",           "2",             "3",             "3"
)
df_out <- dplyr::tribble(
  ~scale_sub1_1, ~scale_sub1_2, ~scale_sub2_1, ~scale_sub2_2, ~scale, ~scale_sub1, ~scale_sub2,
  "1",           "2",           "3",          "3",            9,      3,           6,
  "2",           "3",           "2",          "3",            10,     5,           5,
  "3",           "2",           "3",          "3",            11,     5,           6
)

test_that("Tally with valid characters works", {
  df_result <- df_in %>%
    tally_scale("scale") %>%
    tally_scale("scale_sub1") %>%
    tally_scale("scale_sub2")
  expect_identical(df_result, df_out)
})

df_in <- dplyr::tribble(
  ~scale_sub1_1, ~scale_sub1_2, ~scale_sub2_1, ~scale_sub2_2,
  "uncoercible", "2",             "3",             "3",
  "2",           "3",             "2",             "3",
  "3",           "2",             "3",             "3"
)

test_that("Tally with invalid characters fails", {

  df_in %>%
    tally_scale("scale") %>%
    expect_error
})

# # longer dataframes to examine how printed output looks
# df_in <- data.frame(
#   scale_sub1_1 = c(1, 2, 3),
#   scale_sub1_2 = c(2, 3, 2),
#   scale_sub1_3 = c(0, 0, 0),
#   scale_sub1_4 = c(0, 0, 0),
#   scale_sub1_5 = c(0, 0, 0),
#   scale_sub1_6 = c(0, 0, 0),
#   scale_sub1_7 = c(0, 0, 0),
#   scale_sub1_8 = c(0, 0, 0),
#   scale_sub1_9 = c(0, 0, 0),
#   scale_sub1_10 = c(0, 0, 0),
#   scale_sub2_1 = c(3, 2, 3),
#   scale_sub2_2 = c(3, 3, 3)
# )
# df_out <- data.frame(
#   scale_sub1_1 = c(1, 2, 3),
#   scale_sub1_2 = c(2, 3, 2),
#   scale_sub1_3 = c(0, 0, 0),
#   scale_sub1_4 = c(0, 0, 0),
#   scale_sub1_5 = c(0, 0, 0),
#   scale_sub1_6 = c(0, 0, 0),
#   scale_sub1_7 = c(0, 0, 0),
#   scale_sub1_8 = c(0, 0, 0),
#   scale_sub1_9 = c(0, 0, 0),
#   scale_sub1_10 = c(0, 0, 0),
#   scale_sub2_1 = c(3, 2, 3),
#   scale_sub2_2 = c(3, 3, 3),
#   scale = c(9, 10, 11),
#   scale_sub1 = c(3, 5, 5),
#   scale_sub2 = c(6, 5, 6)
# )
