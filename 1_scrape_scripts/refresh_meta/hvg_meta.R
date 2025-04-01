
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
accessed_time_meta_hvg <- Sys.time()

.board_init_meta <- board_folder(str_c(board_url, "/init_meta"))
.board_refresh_meta <- board_folder(str_c(board_url, "/refresh_meta/hvg"))
.board_init_text <- board_folder(str_c(board_url, "/init_text"))
.board_refresh_text <- board_folder(str_c(board_url, "/refresh_text/hvg"))


# Previous data reading
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

prev_num <- 
  pin_read(
    .board_refresh_meta |> 
      pin_list() |> 
      last(),
    board = .board_refresh_meta) |> 
  pluck("pagenum_hvg") |> 
  as.numeric()

prev_urls <- select(hvg_meta_hist, url)


# Current page number

pagenum_hvg <- 
  read_html(
    "https://hvg.hu/gazdasag/") %>% 
  html_elements(".page") %>% 
  html_text() %>% 
  last() |> 
  as.numeric()


# Scraping

scrape_urls <- paste0("https://hvg.hu/gazdasag/", 
                      1:(pagenum_hvg-prev_num+50), #Csak a biztos letöltés miatt
                      "?ver=1")

cli_alert_info("HVG Meta scraping starting!")
cl <- create_cluster(n.cores = n.cores)
clusterExport(cl, "scrape_urls")
clusterExport(cl, "get_news_meta")

news_meta_hvg <- 
  parLapply(cl, scrape_urls, 
            fun=function(u) get_news_meta(u,
                                          time_node = ".column-articlelist time",
                                          url_node = ".articlelist-element .heading-3 a",
                                          url_attr = "href"
            )) %>%
  reduce(rbind) %>% 
  tibble() %>% 
  mutate(url = str_c("https://hvg.hu", url),
         time_date = 
           lubridate::ymd_hm(time, locale = Sys.getlocale("LC_TIME")),
         time_month = lubridate::floor_date(time_date, "months"))

stopCluster(cl)
cli_alert_info("HVG Meta scraping ended!")
# Refreshing data

news_meta_hvg_new <-
  anti_join(news_meta_hvg, prev_urls)


# Writing pin
if (nrow(news_meta_hvg_new) != 0){
  .board_refresh_meta |>
    pin_write(
      list(
        accessed_time = accessed_time_meta_hvg,
        pagenum_hvg = pagenum_hvg,
        data = news_meta_hvg_new
      ),
      str_c("hvg_meta", janitor::make_clean_names(accessed_time_meta_hvg)),
      versioned = T
    )
  cli_alert_success(
    str_c("New file has been created! ",
    str_c("hvg_meta", janitor::make_clean_names(accessed_time_meta_hvg)),
    " with ", nrow(news_meta_hvg_new), " new rows!"
    ))
} else {
  cli_alert_warning("No new files in HVG!")
}
