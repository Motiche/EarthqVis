library(dplyr)
test_that("eq_clean_data works", {
  test_data =
    readr::read_delim(raw_data, show_col_types = F) %>%
    eq_clean_data()

  expect_that(class(test_data$DATE), equals("Date"))
})

