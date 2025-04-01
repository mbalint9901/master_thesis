library(tidyverse)
library(rvest)
library(currr)
library(purrr)
library(granatlib)
library(pins)

source("00_board_setup.R")
source("00_scrape_functions.R")

accessed_time_text_hvg <- Sys.time()

tibble("pins" = 
         .board |> 
         pins::pin_list()
) |> 
  filter(str_starts(pins, "hvg_meta")) |> 
  mutate(
    data = map(pins, read_data)
  ) |> 
  unnest(data) |> 
  arrange(desc(time_date)) |> 
  select(-pins) |> 
  unique()


news_text_list <- 
  pin_read(.board, "hvg_meta") %>% 
  pluck("data") %>% 
  pull(url)

cl <- create_cluster(n.cores)
clusterExport(cl, "news_text_list")
clusterExport(cl, "get_text")

news_text_hvg <- parLapply(cl, news_text_list, 
                           fun=get_text, 
                           html_node = ".entry-content p, .entry-content,
                              .entry-content li") %>% 
  reduce(rbind) %>% 
  tibble()

stopCluster(cl)

.board |>
  pin_write(
    list(
      accessed_time = accessed_time_text_hvg,
      data = news_text_hvg
    ),
    "hvg_text",
    versioned = T
  )

