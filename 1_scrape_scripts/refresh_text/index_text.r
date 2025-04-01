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

accessed_time_text_index <- Sys.time()

.board_init_meta <- board_folder(str_c(board_url, "/init_meta"))
.board_refresh_meta <- board_folder(str_c(board_url, "/refresh_meta/index"))
.board_init_text <- board_folder(str_c(board_url, "/init_text"))
.board_refresh_text <- board_folder(str_c(board_url, "/refresh_text/index"))

index_meta_hist <- 
  tibble("board" = 
           c(".board_init_meta",
             ".board_refresh_meta")) |> 
  mutate(
    pins = map(board, ~pin_list(get(.x)))
  ) |> unnest(pins) |>
  filter(str_starts(pins, "index_meta")) |> 
  mutate(
    data = map2(pins, board,  ~read_data(dataframe_name = .x, board = get(.y)))
  ) |> 
  unnest(data) |> 
  arrange(desc(time_date)) |> 
  select(-board, -pins) |> 
  unique()

index_text_hist <-
  tibble("board" = 
           c(".board_init_text",
             ".board_refresh_text")) |> 
  mutate(
    pins = map(board, ~pin_list(get(.x)))
  ) |> unnest(pins) |> 
  filter(str_starts(pins, "index_text")) |> 
  mutate(
    data = map2(pins, board,  ~read_data(dataframe_name = .x, board = get(.y)))
  ) |> 
  unnest(data) |> 
  select(-board, -pins) |> 
  unique()


news_text_list <- 
  anti_join(index_meta_hist, index_text_hist) |> 
  pull(url)

if (length(news_text_list) > 0){
  
  cli_alert_info("Index Text scraping starting!")
news_text_index <- 
 news_text_list |> 
    tibble() |>
    mutate(data = 
             map(.progress = T, news_text_list, get_text, 
                 html_node = ".cikk-torzs>p, .cikk-torzs>blockquote>p")
           ) |>
    unnest(data) |>
    select(-news_text_list)
  cli_alert_info("Index Text scraping ended!")
  
  
.board_refresh_text |>
  pin_write(
    list(
      accessed_time = accessed_time_text_index,
      data = news_text_index
    ),
    str_c("index_text", janitor::make_clean_names(accessed_time_text_index)),
    versioned = T
  )
cli_alert_success(
  str_c("New file has been created! ",
        str_c("index_text", janitor::make_clean_names(accessed_time_text_index)),
        " with ", nrow(news_text_index), " new articles."))
} else {
  cli_alert_warning(
    "No new files!"
  )
}