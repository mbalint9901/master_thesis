library(pins)
library(parallel)

n.cores <- detectCores()

create_cluster <- function(n.cores){
  k <- makeCluster(n.cores)
  clusterEvalQ(k, {
    library(tidyverse)
    library(rvest)
    library(cli)
  })
  return(k)
}

get_news_meta_attr <- function(x, url_node, url_attr, time_node, time_attr = ""){
  Sys.sleep(0.1)
  page <- tryCatch(read_html(x),
                   error = function(e) NA)
  tibble(
    time = html_nodes(page, time_node) |>
      html_attr(time_attr) |>
      reduce(c),
    url = html_nodes(page, url_node) |>
      html_attr(url_attr) |>
      reduce(c)
  )
}


get_news_meta <- function(x, url_node, url_attr, time_node){
  y = 1
  Sys.sleep(0.1)
  repeat{
  page <- tryCatch(read_html(x),
                   error = function(e) NA)
  if (is.na(page) & (y < 5)){
    cli_alert_danger(str_c("Website has triggered warning, waiting 10 secs to restart!; Try: ", y))
    Sys.sleep(10)
    y = y+1
  } else {
    ret <- tibble(
      time = html_nodes(page, time_node) |>
        html_text()|>
        reduce(c),
      url = html_nodes(page, url_node) |>
        html_attr(url_attr) |>
        reduce(c)
    )
    return(ret)
    break
  }
  if(y >= 5){
    ret <- tibble(time = NA, url = NA)
    return(ret)
    break
  }
  }
}


get_text <- function(x, html_node) {
  y = 1
  repeat{
  text <- tryCatch(
    read_html(x) %>%
      html_nodes(html_node) %>%
      html_text() %>%
      str_flatten(" "),
    error = function(e) NA)
  if (is.na(text) & (y < 5)){
    cli_alert_danger(str_c("Website has triggered warning, waiting 10 secs to restart!; Try: ", y))
    Sys.sleep(10)
    y = y+1
  } else {
    ret <- tibble(url = x, text)
    return(ret)
    break
  }
  if(y >= 5){
    ret <- tibble(url = NA, text = NA)
    return(ret)
    break
  }
  }
}



