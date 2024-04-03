rm(list=ls())
library(tidyverse)
library(polite)
library(rvest)

url <- "https://www.imdb.com/title/tt15239678/?ref_=hm_fanfav_tt_t_1_pd_fp1_r"

strip_url <- function(url){
  str_extract(url, "/title/tt[0-9]*/")
}

get_page <-function(link){
  politely(read_html)(link)
}

get_links <- function(rel_link){
  full_link <- paste0("https://www.imdb.com", rel_link)
  page <- get_page(full_link)
  links <- page %>% html_elements("a") %>% html_attr("href")
  links <- links[str_detect(links, "^/title/tt")]
  links <- sapply(links, strip_url)
  # strip names off vector
  names(links) <- NULL
  links <- unique(links)
  count <- length(links)
  title <- page %>%
    html_element("title") %>% 
    html_text() %>% 
    str_remove(" - IMDb")
  return(list(title = title,
              link_count=count, 
              rel_links=links))
}

Dune2 <- get_links(strip_url(url))

out <- tibble(
  title = Dune2$title,
  link_count = Dune2$link_count,
  links = Dune2$rel_links)

