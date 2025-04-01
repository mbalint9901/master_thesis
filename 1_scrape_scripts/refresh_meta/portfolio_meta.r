library(tidyverse)
library(rvest)
library(currr)
library(purrr)
library(granatlib)
library(pins)

source("00_board_setup.R")
source("00_scrape_functions.R")

accesed_time <- Sys.time()
start_time <- "1999-02-29"
now <- format(as.Date(Sys.Date()), "%Y-%m-%d")

search_url <- paste0("https://www.portfolio.hu/kereses?q=&a=&df=",
                     start_time, "&dt=",
                     now, "&c=gazdasag"
)

pagenum_portfolio <- 
  read_html(
    search_url) %>% 
  html_elements(".page-link") %>% 
  html_text() %>% 
  nth(-2)

scrape_urls <- paste0(
  str_c(search_url, "&page="), 1:pagenum_portfolio
)

get_news_meta <- function(x) {
    Sys.sleep(0.1)
    # closeAllConnections()
  page <- tryCatch(read_html(x),
                   error = function(e) NA)
  tibble(
    time = html_nodes(page, ".time time") |>
      html_attr("datetime") |>
      reduce(c),
    url = html_nodes(page, ".article-lists .row a") |>
      html_attr("href") |>
      unique() |>
      reduce(c)
  )
}


library(currr)


library(parallel)

n.cores <- detectCores()

cl <- makeCluster(n.cores)
clusterEvalQ(cl, {
  library(tidyverse)
  library(rvest)
})
clusterExport(cl, "scrape_urls")
clusterExport(cl, "get_news_meta")
portfolio_news_meta_df <- parLapply(cl, scrape_urls, fun=get_news_meta) %>% 
  reduce(rbind)

stopCluster(cl)

# 
# 
# portfolio_news_meta_df <- 
#   cp_map(
#     scrape_urls, get_news_meta,
#     name = "portfolio_meta"
#   ) %>% 
#   bind_rows()
# 


.board |>
  pin_write(
    list(
      accesed_time = accesed_time,
      pagenum = pagenum_portfolio,
      data = portfolio_news_meta_df
    ),
    "portfolio_meta"
  )

.board <- board_folder(path = "C:/Users/mbali/Documents/Saját")
