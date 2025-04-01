set.seed(2023)

suppressPackageStartupMessages({
library(pins)
library(tidyverse)
})


source("00_board_setup.R")

.board_data <- board_folder(str_c(board_url, "/data"))

df_total_epu <- read_data("df_total_epu",
                          .board_data)

df_total_epu %>% 
  unnest(cols = c(data_total)) %>% 
  mutate(time_year = lubridate::floor_date(ymd(time_month), "years")) %>% 
  filter(time_year >= 2007) %>% 
  group_by(
    newspaper_list, time_year, EPU_bool) %>% 
  sample_frac(0.03) %>% 
  ungroup() %>% 
  write.csv2(file = "2_results/samples/article_samples.csv",
             fileEncoding = "Windows-1252",
             sep = ";",
             dec = ",")
