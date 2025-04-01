library(tidyverse)
library(rvest)
library(currr)
library(purrr)
library(granatlib)
library(pins)

source("00_board_setup.R")
source("00_scrape_functions.R")

accessed_time_text_vg <- Sys.time()

news_text_list <- pin_read(.board, "vg_meta") %>% 
  pluck("data") %>% 
  pull(url)

cl <- create_cluster(n.cores)
clusterExport(cl, "news_text_list")
clusterExport(cl, "get_text")

set.seed(2023)

news_text_vg <- parLapply(cl, sample(news_text_list, size = 150000), 
                          fun=get_text, 
                          html_node = 
                            ".article-content p") %>% 
  reduce(rbind) %>% 
  tibble()

stopCluster(cl)

.board |>
  pin_write(
    list(
      accessed_time = accessed_time_text_vg,
      data = news_text_vg
    ),
    "vg_text"
  )
