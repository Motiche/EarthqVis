test_that("eq_location_clean works", {
    loc <- readr::read_delim(raw_data,
                             delim = "\t") %>%
      eq_location_clean()
    expect_that(class(loc)[1], equals("spec_tbl_df"))
})
  
