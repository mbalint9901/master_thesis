list.of.packages <- c("pins", 
                      "currr",
                      "parallel",
                      "tidyverse",
                      "rvest",
                      "currr",
                      "purrr",
                      "pins",
                      "cli",
                      "readxl",
                      "writexl",
                      "stringr",
                      "lubridate",
                      "roll",
                      "janitor",
                      "rstudioapi",
                      "rmarkdown"
                      )

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

message('Installed required packages!')
