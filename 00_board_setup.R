library(pins)
library(currr)

board_url <- "0_RData/EPU data"

.board <- board_folder(path =
                         board_url)

options(currr.workers = 8)


read_data <- function(dataframe_name, board, version = NULL){
  board %>%  pin_read(dataframe_name, version = version) %>% 
    .[["data"]]
}

