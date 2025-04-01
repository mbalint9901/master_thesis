library(tidyverse)
library(rvest)
library(currr)
library(purrr)
library(granatlib)
library(pins)

source("00_board_setup.R")
source("00_scrape_functions.R")
accessed_time_meta_index <- Sys.time()


start_time <- "2000-01-01"
stop_time <- "2023-01-02"
now <- format(as.Date(Sys.Date()), "%Y-%m-%d")

search_url <- paste0("https://index.hu/24ora/gazdasag/?tol=",
                     start_time, # start day
                     "&ig=",
                     now,
                     "&rovat=gazdasag&pepe=1&word=1"
)


scrape_urls <- read_html(search_url) %>%
  html_elements(".found") %>%
  html_text %>%
  str_extract("\\d+") %>%
  as.numeric |>
  (\(x) str_c(search_url, '&p=', 0:(x %/% 60))) ()


cl <- create_cluster()
clusterExport(cl, "scrape_urls")
clusterExport(cl, "get_news_meta")

news_meta_index <- 
  parLapply(cl, scrape_urls, 
            fun=~get_news_meta(., 
                               time_node = ".cikk-date-label",
                               url_node = ".cim a",
                               url_attr = "href"
            )) %>% 
  reduce(rbind) %>% 
  tibble() %>% 
  mutate_all(str_squish) %>%
  mutate(
    time = 
      case_when(
        str_detect(time, "^tegnap") ~
          str_c(as.character(as.Date(accessed_time_meta_index)), " 00:00"),
        str_detect(time, "^ma") ~
          str_c(as.character(as.Date(accessed_time_meta_index)), " 00:00"),
        !str_detect(time, "^[0-9]{4}") & 
          !str_detect(time, "^tegnap|^ma") ~ str_c("2023. ", time),
        TRUE ~ time),
    time_date = 
      lubridate::ymd_hm(time, locale = Sys.getlocale("LC_ALL")),
    time_month = lubridate::floor_date(time_date, "months"))

stopCluster(cl)


.board |>
  pin_write(
    list(
      accessed_time = accessed_time_meta_index,
      data = news_meta_index
    ),
    "index_meta"
  )
