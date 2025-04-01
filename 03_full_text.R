suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
  library(writexl)
})
source("00_board_setup.R")
source("utils.R")

.board_data <- board_folder(str_c(board_url, "/data"))

load_total_data <- function(load_type, newspaper){
  crossing(
    newspaper_list =  c(newspaper),
    type = c(load_type),
    board_dir = 
      c("/init_",
        "/refresh_")
  ) |> 
    mutate(
      board_total = ifelse(
        str_detect(board_dir, "init"),
        str_c(board_url, board_dir, type),
        str_c(board_url, board_dir, type, "/", newspaper_list)
      ),
      pin = 
        map(
          board_total,
          ~pin_list(
            board = board_folder(.x)
          )
        )
    ) |> 
    unnest(pin) |> 
    filter(str_starts(pin, newspaper_list)) |> 
    mutate(data = 
             map2(
               .progress = T,
               board_total, pin,
               ~pin_read(
                 board_folder(.x),
                 .y
               ) |> pluck("data"))) |>
    select(-board_dir, -board_total, -pin, -type)
}

for (i in c("hvg", "vg", "index")){
cli_alert_warning(str_c(i, " started!"))
  write.csv(
    x = 
  load_total_data("text", newspaper = i) |> 
  unnest(data) |> 
  drop_na(text) |> 
  filter(text != "") |> 
  mutate(
    E_bool = 
      str_detect(text, 
                 paste0(gazdasag_szavak, collapse = "|")),
    P_bool = 
      str_detect(text, 
                 paste0(szakpolitika_szavak, collapse = "|")),
    U_bool = 
      str_detect(text, 
                 paste0(bizonytalansag_szavak, collapse = "|")),
    EPU_bool = E_bool & P_bool & U_bool),
    file = str_c("2_results/full_texts/", i, "_full_text.csv"))
cli_alert_success(str_c(i, " done!"))
}

