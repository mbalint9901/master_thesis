suppressPackageStartupMessages({
  library(tidyverse)
  library(rvest)
  library(currr)
  library(purrr)
  library(pins)
  library(cli)
})

source("00_board_setup.R")
source("00_scrape_functions.R")

accessed_time_text_hvg <- Sys.time()

.board_init_meta <- board_folder(str_c(board_url, "/init_meta"))
.board_refresh_meta <- board_folder(str_c(board_url, "/refresh_meta/hvg"))
.board_init_text <- board_folder(str_c(board_url, "/init_text"))
.board_refresh_text <- board_folder(str_c(board_url, "/refresh_text/hvg"))

hvg_meta_hist <- 
  tibble("board" = 
           c(".board_init_meta",
             ".board_refresh_meta")) |> 
  mutate(
    pins = map(board, ~pin_list(get(.x)))
  ) |> unnest(pins) |> 
  filter(str_starts(pins, "hvg_meta")) |> 
  mutate(
    data = map2(pins, board,  ~read_data(dataframe_name = .x, board = get(.y)))
  ) |> 
  unnest(data) |> 
  arrange(desc(time_date)) |> 
  select(-board, -pins) |> 
  unique()

hvg_text_hist <-
  tibble("board" = 
           c(".board_init_text",
             ".board_refresh_text")) |> 
  mutate(
    pins = map(board, ~pin_list(get(.x)))
  ) |> unnest(pins) |> 
  filter(str_starts(pins, "hvg_text")) |> 
  mutate(
    data = map2(pins, board,  ~read_data(dataframe_name = .x, board = get(.y)))
  ) |> 
  unnest(data) |> 
  select(-board, -pins) |> 
  unique()


news_text_list <- 
  anti_join(hvg_meta_hist, hvg_text_hist) |> 
  pull(url)


if (length(news_text_list) > 0){
  cli_alert_info("HVG Text scraping starting!")
  cl <- create_cluster(n.cores)
  clusterExport(cl, "news_text_list")
  clusterExport(cl, "get_text")
  clusterExport(cl, "cli_alert_warning")
  
  news_text_hvg <- parLapply(cl, news_text_list, 
                             fun=get_text, 
                             html_node = ".entry-content p, .entry-content,
                              .entry-content li") %>% 
    reduce(rbind) %>% 
    tibble()
  
  stopCluster(cl)
  cli_alert_info("HVG Text scraping ended!")
  
  .board_refresh_text |>
    pin_write(
      list(
        accessed_time = accessed_time_text_hvg,
        data = news_text_hvg
      ),
      str_c("hvg_text", janitor::make_clean_names(accessed_time_text_hvg)),
      versioned = T
    )
  cli_alert_success(
    str_c("New file has been created! ",
          str_c("hvg_text", janitor::make_clean_names(accessed_time_text_hvg)),
          " with ", nrow(news_text_hvg), " new articles."))
} else {
  cli_alert_warning(
    "No new files!"
  )
}
