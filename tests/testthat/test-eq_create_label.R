library(dplyr)
test_that("eq_create_label works", {
  test_data =
    readr::read_delim(raw_data, show_col_types = F) %>%
    eq_clean_data() %>%
    mutate(popup_text = eq_create_label(.))

  expect_that(class(test_data$popup_text), equals("character"))
})
