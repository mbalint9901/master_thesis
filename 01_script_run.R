suppressPackageStartupMessages({
library(tidyverse)
  library(cli)
})

source("00_board_setup.R")
source("utils.R")

.board_data <- board_folder(str_c(board_url, "/data"))

load_total_data <- function(load_type){
  crossing(
    newspaper_list =  c("index","vg", "hvg"),
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

cli_alert_info("Creating df_total_meta!")
df_total_meta <- 
  load_total_data("meta") |> 
  unnest(data)


cli_alert_info("Creating df_total_text_epu!")
df_total_text_epu <-
  load_total_data("text") |> 
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
    EPU_bool = E_bool & P_bool & U_bool) |>
  select(-text)

cli_alert_info("Creating df_total_epu_aggr!")
df_total_epu_aggr <-
  unique(df_total_text_epu) |> 
  left_join(unique(df_total_meta)) |> 
  drop_na() %>% 
  filter(!(newspaper_list == "vg" &
             str_detect(url, paste0("vg.hu/", c("kozelet", "hirek"), collapse = "|")))
  ) |> 
  group_by(newspaper_list, time_month) %>% 
  summarise(
    n = n(),
    E_ratio = sum(E_bool, na.rm = T)/n(), 
    P_ratio = sum(P_bool, na.rm = T)/n(), 
    U_ratio = sum(U_bool, na.rm = T)/n(), 
    EPU_ratio = sum(EPU_bool, na.rm = T)/n()
  ) %>% 
  ungroup() %>% 
  rbind(
    read_data(
      "mtva_text",
      board_folder(str_c(board_url, "/init_text"))
    ) |> rename(n = n_tot)
  )

.board_data |>
  pin_write(
    list(
      data = df_total_text_epu
    ),
    "df_total_text_epu",
    versioned = TRUE
  )

.board_data |>
  pin_write(
    list(
      data = df_total_meta
    ),
    "df_total_meta",
    versioned = TRUE
  )

.board_data |>
  pin_write(
    list(
      data = df_total_epu_aggr
    ),
    "df_total_epu_aggr",
    versioned = TRUE
  )

