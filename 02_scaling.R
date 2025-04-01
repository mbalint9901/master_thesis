
suppressPackageStartupMessages({
  library(tidyverse)
  library(roll)
  library(janitor)
  library(writexl)
  library(cli)
})
source("00_board_setup.R")
source("utils.R")

.board_data <- board_folder(str_c(board_url, "/data"))

df_total_epu_aggr <- read_data("df_total_epu_aggr",
                               .board_data)

df_total_epu_aggr %>% 
  mutate(time_month = as.Date(time_month)) %>% 
  ggplot(aes(x = year(time_month), y = month(time_month, label = TRUE),
             fill = n <= 10)) +
  geom_tile() +
  ggsci::scale_fill_jco() +
  facet_wrap(~newspaper_list) +
  labs(y = NULL, x = NULL)

df_total_epu_aggr %>% 
  mutate(time_month = as.Date(time_month)) %>% 
  group_by(newspaper_list) %>% 
  summarise(
    min = as.character(min(time_month)),
    max = as.character(max(time_month)),
    n = as.character(sum(n, na.rm = T))
  ) %>% 
  ungroup() %>% 
  pivot_longer(-(1)) %>% 
  ggplot(aes(
    y = newspaper_list,
    x =  name,
    label = value,
    fill = newspaper_list
  )) +
  geom_tile() +
  geom_text() +
  ggsci::scale_fill_jco(alpha = 0.2) +
  scale_x_discrete(limits = rev) +
  theme_minimal()

df_total_epu_aggr %>% 
  filter(n >= 10) %>% 
  group_by(newspaper_list) %>% 
  mutate(
    cumsum_sd = 
      roll_sd(EPU_ratio, width = n(), min_obs = 1)
  ) %>%
  ungroup() %>%
  ggplot(aes(x = as.Date(time_month), y = cumsum_sd, color = newspaper_list)) +
  geom_line() +
  ggsci::scale_color_jco()


newspaper_sd_list <-
  df_total_epu_aggr %>% 
  filter(
    time_month >= as.Date("2003-12-01"),
    time_month <= as.Date("2022-12-01")
  ) %>%
  group_by(newspaper_list) %>% 
  summarise(
    sd = 
      sd(EPU_ratio, na.rm = T)
  ) %>% 
  ungroup()


df_total_epu_scale <-
  df_total_epu_aggr %>% 
  filter(
    time_month >= as.Date("2003-12-01")
  ) %>%
  left_join(newspaper_sd_list) |> 
  mutate(
    EPU_ratio_norm = EPU_ratio/sd
  )


newspaper_mean <- 
  df_total_epu_scale |> 
  filter(
    time_month >= as.Date("2003-12-01"),
    time_month <= as.Date("2022-12-01")
  ) |> 
  group_by(time_month) |> 
  summarise(mean_total = mean(EPU_ratio_norm, na.rm = T)) |> 
  ungroup() |> 
  summarise(mean_total_total = mean(mean_total, na.rm = T)) |> 
  pull(mean_total_total)


df_total_epu_scale_mean <-
  df_total_epu_scale |> 
  group_by(time_month) |> 
  mutate(EPU_total = mean(EPU_ratio_norm)*100/newspaper_mean) |> 
  ungroup()


df_total_epu_scale_mean_table <- 
  df_total_epu_scale_mean |> 
  select(-EPU_ratio, -sd) |> 
  pivot_wider(names_from = "newspaper_list",
              values_from = c("n", "EPU_ratio_norm"))


writexl::write_xlsx(
  df_total_epu_scale_mean_table,
  path = str_c(
    "2_results/report_excel/EPU_data_", 
    janitor::make_clean_names(Sys.time()),
    ".xlsx"
  )
)

cli_alert_info(str_c(
  str_c(
    "EPU_data_", 
    janitor::make_clean_names(Sys.time()),
    ".xlsx"
  ),
  " Excel report created!"))


.board_data |>
  pin_write(
    list(
      data = df_total_epu_scale_mean
    ),
    "df_epu_scale",
    versioned = TRUE
  )

.board_data |>
  pin_write(
    list(
      sd = newspaper_sd_list,
      mean = newspaper_mean
    ),
    "newspaper_mean_sd_list",
    versioned = TRUE
  )

