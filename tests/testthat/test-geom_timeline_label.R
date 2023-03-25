test_that("geom_timeline_label works", {
  ex_data <- readr::read_delim(raw_data,
                               delim = "\t") %>%
    eq_clean_data() %>%
    filter(YEAR>2010) %>%
    filter(COUNTRY %in% c("IRAN","CHINA"))
  
  timeline <- ggplot2::ggplot(data = ex_data, aes(x = ex_data$DATE ,
                                                  y = ex_data$COUNTRY,
                                                  size = ex_data$EQ_PRIMARY,
                                                  alpha = ex_data$DEATHS)) +
    geom_timeline_label(n_max = 20)+
    geom_timeline()
  
  
  expect_that(class(timeline)[2], equals("ggplot"))
})
