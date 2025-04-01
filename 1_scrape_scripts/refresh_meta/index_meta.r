
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
accessed_time_meta_index <- Sys.time()

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

prev_num <- 
  pin_read(
    .board_refresh_meta |> 
      pin_list() |> 
      last(),
    board = .board_refresh_meta) |> 
  pluck("pagenum_index") |> 
  as.numeric()


prev_urls <- select(index_meta_hist, url)


start_time <- "2000-01-01"
now <- format(as.Date(Sys.Date()), "%Y-%m-%d")

search_url <- paste0("https://index.hu/24ora/gazdasag/?tol=",
                     start_time, # start day
                     "&ig=",
                     now,
                     "&rovat=gazdasag&pepe=1&word=1"
)

pagenum_index <- read_html(search_url) %>%
  html_elements(".found") %>%
  html_text %>%
  str_extract("\\d+") %>%
  as.numeric

scrape_urls <- 
  str_c(search_url, '&p=', 
              0:((pagenum_index %/% 60)-((prev_num %/% 60) - 10)))


cli_alert_info("Index Meta scraping starting!")

news_meta_index <- 
  scrape_urls |> 
  tibble() |> 
  mutate(data = 
           map(.progress = T,
               .x = scrape_urls, 
               .f = 
                 ~get_news_meta(.x, time_node = ".cikk-date-label",
                                url_node = ".cim a",
                                url_attr = "href")
           )
  ) |> 
  unnest(data) |> 
  select(-scrape_urls) |>
  mutate_all(str_squish) %>%
  mutate(
    time = 
      case_when(
        str_detect(time, "^tegnap") ~
          str_c(as.character(as.Date(accessed_time_meta_index) - 1 ), " 00:00"),
        str_detect(time, "^ma") ~
          str_c(as.character(as.Date(accessed_time_meta_index)), " 00:00"),
        str_detect(time, "órája$") ~
          str_c(as.character(as.Date(accessed_time_meta_index)), " 00:00"),
        str_detect(time, "perce$") ~
          str_c(as.character(as.Date(accessed_time_meta_index)), " 00:00"),
        !str_detect(time, "^[0-9]{4}") & 
          !str_detect(time, "^tegnap|^ma") ~ str_c("2023. ", time),
        TRUE ~ time),
    time_date = 
      lubridate::ymd_hm(time, locale = Sys.getlocale("LC_TIME")),
    time_month = lubridate::floor_date(time_date, "months"))

cli_alert_info("Index Meta scraping ended!")

news_meta_index_new <-
  anti_join(news_meta_index, prev_urls)

if (nrow(news_meta_index_new) > 0){
  .board_refresh_meta |>
    pin_write(
      list(
        accessed_time = accessed_time_meta_index,
        pagenum_index = pagenum_index,
        data = news_meta_index_new
      ),
      str_c("index_meta", janitor::make_clean_names(accessed_time_meta_index)),
      versioned = T
    )
  cli_alert_success(
    str_c("New file has been created! ",
          str_c("index_meta", janitor::make_clean_names(accessed_time_meta_index)),
          " with ", nrow(news_meta_index_new), " new rows!"))
} else {
  cli_alert_warning("No new files in Index!")
}
