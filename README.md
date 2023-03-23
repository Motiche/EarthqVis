EarthqVis
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# EarthqVis

<!-- badges: start -->
<!-- badges: end -->

The goal of EarthqVis is to visualize earthquke data.

## Installation

You can install the development version of EarthqVis from this Github
Repo.

``` r
devtools::install_github("Motiche/EarthqVis")
```

## Example

This is an example of drawing a timeline:

``` r
library(EarthqVis)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
ex_data <- readr::read_delim(raw_data, show_col_types = F) %>% 
  eq_clean_data () %>% 
  filter(YEAR>2010) %>% 
  filter(COUNTRY %in% c("IRAN","CHINA")) %>% 
  tidyr::drop_na(c("LONGITUDE", "LATITUDE"))
plot <- ggplot(data = ex_data, aes(x = ex_data$DATE ,
                           y = ex_data$COUNTRY,
                           size = ex_data$EQ_PRIMARY,
                           alpha = ex_data$DEATHS,
                           label = ex_data$LOCATION_NAME)) + 
  
  geom_timeline_label(n_max = 20)+
  geom_timeline()+
  theme_light()+
  labs(size = "Strength (Richter)",
       alpha = "Number of deaths")+
  xlab("Time") + ylab("")
```

This is an example of showing on map:

``` r
library(EarthqVis)
library(dplyr)
library(ggplot2)
ex_data <- readr::read_delim(raw_data, show_col_types = F) %>% 
  eq_clean_data () %>% 
  filter(YEAR>1900) %>% 
  filter(COUNTRY %in% c("IRAN")) %>% 
  tidyr::drop_na(c("LONGITUDE", "LATITUDE"))
map <- ex_data %>% 
  eq_clean_data() %>%
     dplyr::mutate(popup_text = eq_create_label(.)) %>% 
     eq_map(annot_col = "popup_text")
```
