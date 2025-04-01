library(tidyverse)
library(rvest)
library(currr)
library(purrr)
library(cli)
library(pins)
library(readxl)

source("00_board_setup.R")
source("00_scrape_functions.R")

accessed_time_text_mtva <- Sys.time()

.board_init_text <- board_folder(str_c(board_url, "/init_text"))

data <- 
  list.files("0_RData/MTVA_adatok/txt", full.names = T) |> 
  lapply(
    function(x) ifelse(str_detect(x, "Makronóm2023"), 
                       read_file(x, locale = locale(encoding = "Windows-1250"))|> str_remove_all("\r\n"),
                       read_file(x, locale = locale(encoding = "UTF-8")) |> str_remove_all("\r\n")
    )
  ) |> reduce(c)


cli_alert_info("MTVA Text collection starting!")

data_kiadas_uno <- 
  tibble(kiadas_datum = 
           str_extract_all(data, 
                           paste(
                             "Uno:[0-9]+:",
                             c("MTI", "OSS"),
                             ":[A-z][0-9]+Kiadás dátuma:[0-9\\.:]+ [0-9\\.:]+",
                             sep = "", collapse = "|")) |> 
           reduce(c)
  ) |> 
  mutate(kiadas_ido =
           lubridate::ymd_hms(str_extract(kiadas_datum, "[0-9\\.:]+ [0-9\\.:]+")),
         kiadas_honap = floor_date(kiadas_ido, "months")
  ) |> 
  unique()

data_count <- data_kiadas_uno |> 
  filter(str_detect(kiadas_datum, "Uno:[0-9]{8}:[A-Z]{3}:[zZ]")) |>
  group_by(kiadas_honap) |> 
  summarise(n = n())
  

data_total <- 
  lapply(excel_sheets("0_RData/MTVA_adatok/Makronom_excel.xlsx"),
    function(x) readxl::read_xlsx("0_RData/MTVA_adatok/Makronom_excel.xlsx", sheet = x) |> 
      pivot_longer(-1) |> 
      (\(x) mutate(x, ev = names(x)[1]))() |> 
      select("honap" = 1, name, value, ev)
  ) |> 
  reduce(bind_rows) |> 
  filter(name == "Gazdasági hírek") |>
  transmute(kiadas_honap = as.Date(str_c(ev, " ", honap, " 1."),
                                   format = "%Y. %B %d"),
            n_tot = value)
cli_alert_info("MTVA Text collection ended!")


mtva_epu <- data_count |> left_join(data_total) |> 
    transmute(
      newspaper_list = "mtva",
      time_month = kiadas_honap,
      n_tot,
      EPU_ratio = n/n_tot)
  
if(nrow(mtva_epu) > 0){
.board_init_text |>
  pin_write(
    list(
      accessed_time = accessed_time_text_mtva,
      data = mtva_epu
    ),
    "mtva_text",
    versioned = T
  )
  cli_alert_success("MTVA sucessfully refreshed!")
} else {
  cli_alert_warning("MTVA refresh has 0 rows!")
}