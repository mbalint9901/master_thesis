library(tidyverse)
library(rvest)
library(pins)
library(parallel)

source("00_board_setup.R")
source("00_scrape_functions.R")

accessed_time_meta_vg <- Sys.time()

pagenum_vg <- read_html("https://www.vg.hu/kereses") %>%
  html_elements(".pagenum") %>%
  html_text %>%
  last() %>%
  as.numeric

scrape_urls <- paste0('https://www.vg.hu/kereses?page=', 1:pagenum_vg)

cl <- create_cluster()
clusterExport(cl, "scrape_urls")
clusterExport(cl, "get_news_meta")

news_meta_vg <- 
  parLapply(cl, scrape_urls, 
            fun=~get_news_meta(., 
                               time_node = ".info",
                               url_node = ".title-link",
                               url_attr = "href"
            )) %>% 
  reduce(rbind) %>% 
  tibble() %>% 
  mutate_all(str_squish) %>%
  mutate(
    url = str_c("https://www.vg.hu", url),
    time = 
      case_when(
        str_detect(time, "^Tegnap") ~
          str_c(as.character(as.Date(accessed_time_meta_vg) - 1), " 00:00"),
        str_detect(time, "órája|perce") ~
          str_c(as.character(as.Date(accessed_time_meta_vg)), " 00:00"),
        TRUE ~ 
          str_c(str_extract(time, "^[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}"),
                ". 00:00")),
    time_date = 
      lubridate::ymd_hm(time, locale = Sys.getlocale("LC_ALL")),
    time_month = lubridate::floor_date(time_date, "months"))

stopCluster(cl)

.board |>
  pin_write(
    list(
      accessed_time = accessed_time_meta_vg,
      pagenum = pagenum_vg,
      data = news_meta_vg
    ),
    "vg_meta"
  )
