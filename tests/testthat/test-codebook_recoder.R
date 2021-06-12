

test_that("Basic recoding works", {
  df_data <- dplyr::tribble(
    ~scale_sub1_1, ~scale_sub1_2, ~scale_sub2_1, ~scale_sub2_2,
    "one",           "two",             "thr",             "thr",
    "two",           "thr",             "two",             "thr",
    "thr",           "two",             "thr",             "thr"
  )

  df_codebook <- dplyr::tribble(
    ~var_regex, ~values_from, ~values_to,
    "scale_",   "one",       "1",
    "scale_",   "two",       "2",
    "scale_",   "thr",       "3"
  )

  df_output <- dplyr::tribble(
    ~scale_sub1_1, ~scale_sub1_2, ~scale_sub2_1, ~scale_sub2_2,
    1,             2,             3,             3,
    2,             3,             2,             3,
    3,             2,             3,             3
  )


  df_recoded <- df_data %>%
    codebook_recoder(var_regex=df_codebook$var_regex,
                     values_from=df_codebook$values_from,
                     values_to=df_codebook$values_to)
  expect_identical(df_recoded, df_output)
})

# TODO: add more tests

# test if vector lengths differ
# look into why nature project wasn't working and test taht
