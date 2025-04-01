library(tidyverse)
library(rvest)
library(currr)
library(purrr)
library(granatlib)
library(pins)

source("00_board_setup.R")
source("00_scrape_functions.R")
accessed_time_meta_hvg <- Sys.time()

pagenum_hvg <- 
  read_html(
    "https://hvg.hu/gazdasag/") %>% 
  html_elements(".page") %>% 
  html_text() %>% 
  last()

scrape_urls <- paste0("https://hvg.hu/gazdasag/", 
                      1:pagenum_hvg,
                      "?ver=1")

cl <- create_cluster(n.cores = n.cores)
clusterExport(cl, "scrape_urls")
clusterExport(cl, "get_news_meta")

news_meta_hvg <- 
  parLapply(cl, scrape_urls, 
            fun=get_news_meta, 
                               time_node = ".column-articlelist time",
                               url_node = ".articlelist-element .heading-3 a",
                               url_attr = "href"
            ) %>% 
  reduce(rbind) %>% 
  tibble() %>% 
  mutate(url = str_c("https://hvg.hu", url),
         time_date = 
           lubridate::ymd_hm(time, locale = Sys.getlocale("LC_ALL")),
         time_month = lubridate::floor_date(time_date, "months"))


stopCluster(cl)

.board |>
  pin_write(
    list(
      accessed_time = accessed_time_meta_hvg,
      pagenum_hvg = pagenum_hvg,
      data = news_meta_hvg
    ),
    "hvg_meta",
    versioned = T
  )
