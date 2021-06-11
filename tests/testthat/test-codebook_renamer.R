
df_data <- dplyr::tribble(
  ~scale_sub1_1, ~scale_sub1_2, ~scale_sub2_1, ~scale_sub2_2,
  1,             2,             3,             3,
  2,             3,             2,             3,
  3,             2,             3,             3
)

df_codebook <- dplyr::tribble(
  ~col_name_from, ~col_name_to,
  "scale_sub1_1", "sc_s1_1",
  "scale_sub1_2", "sc_s1_2",
  "scale_sub2_1", "sc_s2_1",
  "scale_sub2_2", "sc_s2_2"
)

test_that("Basic renaming works", {
  df_renamed <- df_data %>%
    codebook_renamer(df_codebook$col_name_from, df_codebook$col_name_to)
  expect_identical(names(df_renamed), df_codebook$col_name_to)
})



df_data <- dplyr::tribble(
  ~scale_sub1_1, ~scale_sub1_2, ~scale_sub2_1, ~scale_sub2_2,
  1,             2,             3,             3,
  2,             3,             2,             3,
  3,             2,             3,             3
)

df_codebook <- dplyr::tribble(
  ~col_name_from, ~col_name_to,
  "scale_sub1_1", "sc_s1_1",
  "scale_sub1_2", "sc_s1_2",
  "scale_sub2_1", "sc_s2_1",
  "scale_sub2_2", "sc_s2_2",
  "missing_col1" , "m_col1",
  "missing_col2" , "m_col2"
)
test_that("Data columns missing from codebook: Works with warning", {
  expect_warning({df_renamed <- df_data %>%
    codebook_renamer(df_codebook$col_name_from, df_codebook$col_name_to)})
  expect_true(all(names(df_renamed) %in% df_codebook$col_name_to &
                    !names(df_renamed) %in% df_codebook$col_name_from))
})


df_data <- dplyr::tribble(
  ~scale_sub1_1, ~scale_sub1_2, ~scale_sub2_1, ~scale_sub2_2, ~norename1, ~norename2,
  1,             2,             3,             3,             2,          3,
  2,             3,             2,             3,             3,          2,
  3,             2,             3,             3,             2,          3
)

df_codebook <- dplyr::tribble(
  ~col_name_from, ~col_name_to,
  "scale_sub1_1", "sc_s1_1",
  "scale_sub1_2", "sc_s1_2",
  "scale_sub2_1", "sc_s2_1",
  "scale_sub2_2", "sc_s2_2"
)
test_that("Codebook entries missing from data: Works with warning", {
  expect_warning({df_renamed <- df_data %>%
    codebook_renamer(df_codebook$col_name_from, df_codebook$col_name_to)})
  expect_true(all(df_codebook$col_name_to %in% names(df_renamed) &
                    !df_codebook$col_name_from %in% names(df_renamed)))
})

