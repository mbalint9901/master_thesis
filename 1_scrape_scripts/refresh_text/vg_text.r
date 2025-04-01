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

accessed_time_text_vg <- Sys.time()

.board_init_meta <- board_folder(str_c(board_url, "/init_meta"))
.board_refresh_meta <- board_folder(str_c(board_url, "/refresh_meta/vg"))
.board_init_text <- board_folder(str_c(board_url, "/init_text"))
.board_refresh_text <- board_folder(str_c(board_url, "/refresh_text/vg"))

vg_meta_hist <- 
  tibble("board" = 
           c(".board_init_meta",
             ".board_refresh_meta")) |> 
  mutate(
    pins = map(board, ~pin_list(get(.x)))
  ) |> unnest(pins) |> 
  filter(str_starts(pins, "vg_meta")) |> 
  mutate(
    data = map2(pins, board,  ~read_data(dataframe_name = .x, board = get(.y)))
  ) |> 
  unnest(data) |> 
  arrange(desc(time_date)) |> 
  select(-board, -pins) |> 
  unique()

vg_text_hist <-
  tibble("board" = 
           c(".board_init_text",
             ".board_refresh_text")) |> 
  mutate(
    pins = map(board, ~pin_list(get(.x)))
  ) |> unnest(pins) |> 
  filter(str_starts(pins, "vg_text")) |> 
  mutate(
    data = map2(pins, board,  ~read_data(dataframe_name = .x, board = get(.y)))
  ) |> 
  unnest(data) |> 
  select(-board, -pins) |> 
  unique()


news_text_list <- 
  anti_join(vg_meta_hist, vg_text_hist) |> 
  filter(lubridate::ymd_hm(time) >= as.Date("2023-02-02")) |>
  filter(!str_detect(url, paste0("vg.hu/", c("kozelet", "hirek"), collapse = "|"))) |> 
  pull(url)


if (length(news_text_list) > 0){
  cli_alert_info("VG Text scraping started!")
  news_text_vg  <- 
    news_text_list |> 
    tibble() |>
    mutate(data = 
             map(.progress = T, news_text_list, get_text, 
                 html_node = ".article-content p")
    ) |> 
    unnest(data) |> 
    select(-news_text_list)
  cli_alert_info("VG Text scraping ended!")
  # cl <- create_cluster(n.cores)
  # clusterExport(cl, "news_text_list")
  # clusterExport(cl, "get_text")
  # clusterExport(cl, "cli_alert_warning")
  # 
  # news_text_vg <- parLapply(cl, news_text_list, 
  #                            fun=get_text, 
  #           html_node = 
  #             ".article-content p") %>% 
  #   reduce(rbind) %>% 
  #   tibble()
  # 
  # stopCluster(cl)
  
  .board_refresh_text |>
    pin_write(
      list(
        accessed_time = accessed_time_text_vg,
        data = news_text_vg
      ),
      str_c("vg_text", janitor::make_clean_names(accessed_time_text_vg)),
      versioned = T
    )
  cli_alert_success(
    str_c("New file has been created! ",
          str_c("vg_text", janitor::make_clean_names(accessed_time_text_vg)),
          " with ", nrow(news_text_vg), " new articles."))
} else {
  cli_alert_warning(
    "No new files!"
  )
}
