library(tidyverse)
library(rvest)
library(currr)
library(purrr)
library(granatlib)
library(pins)

source("00_board_setup.R")
source("00_scrape_functions.R")

accessed_time_text_index <- Sys.time()

news_text_list <- pin_read(.board, "index_meta") %>% 
  pluck("data") %>% 
  pull(url)


cl <- create_cluster(n.cores)
clusterExport(cl, "news_text_list")
clusterExport(cl, "get_text")

news_text_index <- parLapply(cl, news_text_list, 
                              fun=get_text, 
                              html_node = ".cikk-torzs>p, .cikk-torzs>blockquote>p") %>% 
  reduce(rbind) %>% 
  tibble()

stopCluster(cl)

.board |>
  pin_write(
    list(
      accessed_time = accessed_time_text_index,
      data = news_text_index
    ),
    "index_text"
  )
