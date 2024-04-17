rm(list=ls())
library(tidyverse)
library(polite)
library(rvest)

url <- "https://www.imdb.com/title/tt15239678/?ref_=hm_fanfav_tt_t_1_pd_fp1_r"

strip_url <- function(url){
  str_extract(url, "/title/tt[0-9]*/")
}

build_url <- function(rel_url){
  paste0("https://www.imdb.com", rel_url)
}

get_page <-function(url){
  politely(read_html)(url)
}

get_urls <- function(rel_url){
  full_url <- build_url(rel_url)
  page <- get_page(full_url)
  urls <- page %>% html_elements("a") %>% html_attr("href")
  url_titles <- page %>% html_elements("a") %>% html_text()
  urls <- urls[str_detect(urls, "^/title/tt")]
  url_titles <- url_titles[str_detect(urls, "^/title/tt")]
  urls <- sapply(urls, strip_url)
  # strip names off vector
  names(urls) <- NULL
  urls <- unique(urls)
  url_titles <- unique(url_titles)
  count <- length(urls)
  title <- page %>%
    html_element("title") %>% 
    html_text() %>% 
    str_remove(" - IMDb")
  url <- rel_url
  return(data.frame(title = title,
              url_count=count, 
              rel_urls=urls,
              link_titles=url_titles,
              url = url))
}

Dune2 <- get_urls(strip_url(url))

# note to self: everything above works almost as expected.
# todo: get rid of links to top 250 movies, press room, etc.
# todo: get links to people (e.g. actors, directors)

out <- tibble(
  title = Dune2$title,
  url_count = Dune2$url_count,
  urls = Dune2$rel_urls)

tried_urls <- out$urls
out2 <- lapply(tried_urls, get_urls)

out3 <- tibble(
  title = NULL,
  url_count = NULL,
  url = NULL
)

for(item in out2){
  df <- data.frame(title = item$title,
                   url_count = item$url_count,
                   url = item$url)
  out3 <- full_join(out3, df)
}


write_csv(out3, "challenge11_answer.csv")