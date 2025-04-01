
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
accessed_time_meta_vg <- Sys.time()

.board_init_meta <- board_folder(str_c(board_url, "/init_meta"))
.board_refresh_meta <- board_folder(str_c(board_url, "/refresh_meta/vg"))
.board_init_text <- board_folder(str_c(board_url, "/init_text"))
.board_refresh_text <- board_folder(str_c(board_url, "/refresh_text/vg"))


# Previous data reading
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

prev_num <-
  pin_read(
    .board_refresh_meta |> 
      pin_list() |> 
      last(),
    board = .board_refresh_meta) |> 
  pluck("pagenum_vg") |> 
  as.numeric()

prev_urls <- select(vg_meta_hist, url)


# Current page number
pagenum_vg <- read_html("https://www.vg.hu/kereses") %>%
  html_elements(".pagenum") %>%
  html_text %>%
  last() %>%
  as.numeric

scrape_urls <- paste0('https://www.vg.hu/kereses?page=', 1:(pagenum_vg-prev_num+50))

cli_alert_info("VG Meta scraping starting!")

cl <- create_cluster(n.cores = n.cores)
clusterExport(cl, "scrape_urls")
clusterExport(cl, "get_news_meta")
clusterExport(cl, "cli_alert_warning")

news_meta_vg <- 
  parLapply(cl, scrape_urls, 
            fun=function(u) get_news_meta(u, 
                               time_node = ".info",
                               url_node = ".title-link",
                               url_attr = "href"
            )) %>% 
  reduce(rbind) %>% 
  tibble() %>% 
  mutate_all(str_squish) %>%
  mutate(
    url = str_c("https://www.vg.hu", url),
    time = 
      case_when(
        str_detect(time, "^Tegnap") ~
          str_c(as.character(as.Date(accessed_time_meta_vg) - 1), " 00:00"),
        str_detect(time, "órája|perce") ~
          str_c(as.character(as.Date(accessed_time_meta_vg)), " 00:00"),
        TRUE ~ 
          str_c(str_extract(time, "^[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}"),
                ". 00:00")),
    time_date = 
      lubridate::ymd_hm(time, locale = Sys.getlocale("LC_TIME")),
    time_month = lubridate::floor_date(time_date, "months")) |> 
  filter(!str_detect(url, paste0("vg.hu/", c("kozelet", "hirek"), collapse = "|")))

stopCluster(cl)
cli_alert_info("VG Meta scraping ended!")


news_meta_vg_new <-
  anti_join(news_meta_vg, prev_urls)


# Writing pin
if (nrow(news_meta_vg_new) != 0){
  .board_refresh_meta |>
    pin_write(
      list(
        accessed_time = accessed_time_meta_vg,
        pagenum_vg = pagenum_vg,
        data = news_meta_vg_new
      ),
      str_c("vg_meta", janitor::make_clean_names(accessed_time_meta_vg)),
      versioned = T
    )
  cli_alert_success(
    str_c("New file has been created! ",
          str_c("vg_meta", janitor::make_clean_names(accessed_time_meta_vg)),
          " with ", nrow(news_meta_vg_new), " new rows!"))
} else {
  cli_alert_warning("No new files in VG!")
}
