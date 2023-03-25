test_that("eq_map works", {
  map <- readr::read_delim(raw_data,
                    delim = "\t") %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
    eq_map(annot_col = "DATE")
  expect_that(class(map)[1], equals("leaflet"))
})
