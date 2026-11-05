cny_query <-
  function(diocese_name = NULL,
           diocese_number = NULL,
           church_code = NULL,
           parish_id = NULL,
           where = ""){

    where_string <- create_where_string(
      diocese_name = diocese_name,
      diocese_number = diocese_number,
      church_code = church_code,
      parish_id = parish_id,
      where = where
    )

    result <- churches("*", where = where_string)

    result <- result[c("Church_Cod", "ACNY_url")]

    names(result) <- c("church_code", "acny_url")

    result$acny_url <- add_trailing_slash(result$acny_url)

    result

  }

cny_query_scrape <- function(scrape_function, ...){

  result <- cny_query(...)

  urls <- result$acny_url

  urls <- urls[!is.na(urls) & !urls %in% c("", " ")]


  events <- scrape_function(urls)
  events$acny_url <- add_trailing_slash(events$acny_url)

  dplyr::left_join(result, events, by = "acny_url")
}

cny_query_events <- function(...){
  cny_query_scrape(cny_scrape_events, ...)
}

cny_query_contact <- function(...){
  cny_query_scrape(cny_scrape_contact, ...)
}

add_trailing_slash <- function(x){
  stringr::str_replace(x, "(?<=\\d)$", "/")
}
