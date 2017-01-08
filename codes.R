library(httr)
library(rvest)
library(dplyr)

## album

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

get_country <- function(id) {
  
  country_link <- function(id, start) {
    l <- paste0('http://www.metal-archives.com/browse/ajax-country/c/',id,'/json/1?iDisplayStart=',start,'&sEcho=1')
    return(l)
  }
  
  start <- 0
  k <-1
  link <- country_link(id = id, start = 0)
  first_page <- GET(link) %>% content()
  json <- list()
  while (start < first_page$iTotalRecords) {
    link <- country_link(id = id, start)
    page <- GET(link) %>% content()
    json[[k]] <- page$aaData %>% jsonlite::toJSON()
    start <- start + 500
    k <- k + 1
  }
  
  json_result <- lapply(json, jsonlite::fromJSON)
  json_result <- lapply(json_result, as.data.frame)
  json_result <- bind_rows(json_result)
  names(json_result) <- c('band_link','band_genre','band_city','band_status')
  json_result <- json_result %>%
    mutate(band_name = stringi::stri_extract(band_link, regex = '>.*<'),
           band_name = stringi::stri_replace(band_name, fixed = '>', rep =''),
           band_name = stringi::stri_replace(band_name, fixed = '<', rep =''),
           band_id = stringi::stri_extract(band_link, regex = "\\d{1,}\\'>"),
           band_id = stringi::stri_extract(band_id, regex = "\\d{1,}"),
           band_status = stringi::stri_extract(band_status, regex = '>.*<'),
           band_status = stringi::stri_replace(band_status, fixed = '>', rep =''),
           band_status = stringi::stri_replace(band_status, fixed = '<', rep ='')) %>%
    select(band_id,band_name,band_genre,band_city,band_status)
  
  return(json_result)
}


get_bands_letter <- function(id,...) {
  link <- paste0('http://em.wemakesites.net/letter/',id,'?api_key=',.api_key)
  page <- GET(link)
  json <- page %>% read_html() %>% html_text()
  json_result <- jsonlite::fromJSON(json,...)
  return(json_result)
}
