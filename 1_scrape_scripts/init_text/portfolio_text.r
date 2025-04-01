library(tidyverse)
library(rvest)
library(currr)
library(purrr)
library(granatlib)
library(pins)

source("00_board_setup.R")
source("00_scrape_functions.R")


library(tidyverse)
library(rvest)
url_in <- "https://www.wsj.com/"
  url <- "https://sso.accounts.dowjones.com/login-page?op=localop&scope=openid%20idp_id%20roles%20email%20given_name%20family_name%20djid%20djUsername%20djStatus%20trackid%20tags%20prts%20suuid%20updated_at&client_id=5hssEAdMy0mJTICnJNvC9TXEw3Va7jfO&response_type=code&redirect_uri=https%3A%2F%2Faccounts.wsj.com%2Fauth%2Fsso%2Flogin&nonce=d3dc54f1-fe5e-48e9-bd51-8a68fad0149a&ui_locales=en-us-x-wsj-223-2&mars=-1&ns=prod%2Faccounts-wsj&state=uOiEi-3gL2l96PQr.7-1So7BT-lU63MJRKIYfRsAkhTy_-CPhVwqRvddS3VM&protocol=oauth2&client=5hssEAdMy0mJTICnJNvC9TXEw3Va7jfO#!/signin"
s <- session(url)

form <- read_html(s) |> 
  html_form() |> 
  first() |> 
  html_form_set(
  username = "mbalint9901@gmail.com",
  password = "Wsjjelszo1234.",
)

form$action <- url_in

res <- html_form_submit(s, form)
  read_html() %>%
  html_elements("section") %>% 
  html_text()

#login
url <- 
  "https://www.portfolio.hu/gazdasag/20230206/bajban-az-orosz-gazdasag-1998-ota-nem-volt-ilyen-pocsek-evkezdet-595262"
session <- session(url)

form <- html_form(read_html(url))[[2]]

filled_form <- html_form_set(form,
                          username = "blazsanik.bernadett@makronomkti.hu",
                          password = "Patriot2020")

x <- html_form_submit(form = filled_form)
k <- read_html(x$content)
read_html(x$content) %>% html_nodes("p+ p , #y-adoceanindexhuspdlqthnru+ p")

portfolio_accesed_time_text <- Sys.time()
now <- format(as.Date(Sys.Date()), "%Y-%m-%d")

news_text_list <- pin_read(.board, "portfolio_meta") %>% 
  pluck("data") %>% 
  pull(url)



cl <- create_cluster(n.cores)
clusterExport(cl, "news_text_list")
clusterExport(cl, "get_text")
portfolio_news_text_df <- 
  parLapply(cl, news_text_list[10000:10040], 
            fun=get_text, 
            html_node = "p+ p , #y-adoceanindexhuspdlqthnru+ p, .fade-text p") %>% 
  reduce(rbind)

stopCluster(cl)


.board |>
  pin_write(
    list(
      accesed_time = portfolio_accesed_time_text,
      data = portfolio_news_text_df
    ),
    "portfolio_text"
  )
