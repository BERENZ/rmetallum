library(httr)
library(jsonlite)
library(rvest)

get_album <- function(id,...) {
  link <- paste0('http://em.wemakesites.net/album/',id,'?api_key=',.api_key)
  page <- GET(link)
  json <- page %>% read_html() %>% html_text()
  json_result <- jsonlite::fromJSON(json,...)
  return(json_result)
}

get_artist <- function(id,...) {
  link <- paste0('http://em.wemakesites.net/artist/',id,'?api_key=',.api_key)
  page <- GET(link)
  json <- page %>% read_html() %>% html_text()
  json_result <- jsonlite::fromJSON(json,...)
  return(json_result)
}

get_band <- function(id,...) {
  link <- paste0('http://em.wemakesites.net/band/',id,'?api_key=',.api_key)
  page <- GET(link)
  json <- page %>% read_html() %>% html_text()
  json_result <- jsonlite::fromJSON(json,...)
  return(json_result)
}

get_country <- function(id,...) {
  
  country_link <- function(id, start) {
    l <- paste0('http://www.metal-archives.com/browse/ajax-country/c/',id,'/json/1?iDisplayStart=',start,'&sEcho=1')
    return(l)
  }
  
  start <- 0
  link <- country_link(id = id, start = 0)
  page <- GET(link)
  
  
  json <- page %>% read_html() %>% html_text()
  
  
  json_result <- jsonlite::fromJSON(json,...)
  return(json_result)
}

get_bands_letter <- function(id,...) {
  link <- paste0('http://em.wemakesites.net/letter/',id,'?api_key=',.api_key)
  page <- GET(link)
  json <- page %>% read_html() %>% html_text()
  json_result <- jsonlite::fromJSON(json,...)
  return(json_result)
}
