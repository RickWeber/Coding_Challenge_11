rm(list=ls())
library(tidyverse)
library(polite)
library(rvest)

url <- "https://www.imdb.com/title/tt15239678/?ref_=hm_fanfav_tt_t_1_pd_fp1_r"

# first_page <- read_html(url)
# 
# links <- first_page %>% 
#   html_elements("a") %>% 
#   html_attr("href")
# 
# links <- links[str_detect(links, "^/title/tt")]
# #links[str_detect(links, "tt15239678")]

strip_url <- function(url){
  str_extract(url, "/title/tt[0-9]*/")
}
# 
# links <- sapply(links, strip_url)
# # strip names off vector
# names(links) <- NULL
# 
# links <- unique(links)

get_page <-function(link){...}

get_more <- function(rel_link){
  full_link <- paste0("https://www.imdb.com", rel_link)
  links <- read_html(full_link) %>% 
    html_elements("a") %>% 
    html_attr("href")
  links <- links[str_detect(links, "^/title/tt")]
  links <- sapply(links, strip_url)
  # strip names off vector
  names(links) <- NULL
  links <- unique(links)
  count <- length(links)
  return(list(count=count, links=links))
}

get_title <- function(rel_link){
  full_link <- paste0("https://www.imdb.com", rel_link)
  read_html(full_link) %>% 
    html_element("title") %>% 
    html_text() %>% 
    str_remove(" - IMDb")
}
