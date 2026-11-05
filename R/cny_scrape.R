## HELPERS ##

## get cny url num from url

cny_url_num <- function(url){
  stringr::str_extract(url, "\\d+")
}

## Create a httr2 request for use by this package
cny_request <- function(acny_url_num){

  httr2::request("https://www.achurchnearyou.com") |>
    httr2::req_url_path("church", acny_url_num) |>
    httr2::req_throttle(capacity = 120, fill_time_s = 60) |>
    httr2::req_user_agent("R Package 'churchnearyou' (in development) https://github.com/church-army/churchnearyou")
}

## Append something to every request in a list
map_append_path <- function(request_list, append_this){

  purrr::map2(request_list, append_this,
              httr2::req_url_path_append)

}

get_text <- function(x) rvest::html_text(x, trim = TRUE)

## EXPORTS ##

#' Get tidy(ish) data on Church Events
#' @export
cny_scrape_events <- function(cny_url){

  ## Perform requests and parse html data -------------------------------------------------

  cny_nums <- cny_url_num(cny_url)

  if(any(is.na(cny_nums))) stop("Missing/unnaceptable URLs provided.")

  requests <- purrr::map(cny_nums, cny_request)
  request_strings <- purrr::map_chr(requests, "url")

  events_req <- map_append_path(requests, "service-and-events/events-all/")

  events_resp <- httr2::req_perform_parallel(events_req, progress = "Obtaining Church Near You Data")

  events_html <- purrr::map(events_resp, httr2::resp_body_html)

  ## Extract 'event cards' for each cny url ------------------------------------------------

  events_data_per_church <-

    purrr::map2(

      events_html, request_strings,

      \(result, url_string){

        events <- rvest::html_elements(result, ".card__summary")

        ## Extract key data points from each 'event card' ::::::::::::::::::::::::::::::::::

        events_data_per_event <-
          purrr::map(events, \(x){

            # title and metadata ---
            card_title <- rvest::html_elements(x, ".card__title")
            card_meta <- rvest::html_elements(x, ".card__meta")

            descriptions <- rvest::html_elements(card_meta, ".meta__description")

            # event name ---
            event_name_text <- get_text(card_title)

            # event description ---
            event_description <-
              rvest::html_elements(
                card_meta,
                ".meta__description.card__event-description"
                )

            event_description_text <- get_text(event_description)

            # venue info ---
            venue <- rvest::html_elements(card_meta, ".meta__venue")

            venue_names <- rvest::html_elements(venue, ".venue")
            venue_addresses <- rvest::html_elements(venue, ".address")

            venue_names_text <- get_text(venue_names)
            venue_addresses_text <- get_text(venue_addresses)

            venue_addreses_text <-
              stringr::str_replace_all(
                venue_addresses_text,
                "\r\n", ", "
                )

            venue_info <-
              tibble::tibble(
                venue_name = venue_names_text,
                venue_address = venue_addresses_text)

            # event time ---
            event_time <- descriptions[1]

            event_timestamp <-
              rvest::html_elements(event_time, "time") |>
              rvest::html_attr("datetime")

            event_time_text <- get_text(event_time)

            # further_information ---
            further_information <- descriptions[-(1:2)]
            further_information_text <- get_text(further_information)

            if(length(further_information) == 0) further_information <- NA_character_

            # event tags ---
            tags <- rvest::html_elements(card_meta, ".tag.tag--event")

            tag_classes <- rvest::html_attr(tags, "class")

            tag_groups <- stringr::str_extract(tag_classes, "[a-z\\-]+$")

            tag_text <- get_text(tags)

            tag_group_list <- as.list(sort(unique(tag_groups)))
            names(tag_group_list) <- tag_group_list

            tag_group_list <-
              purrr::map(tag_group_list,
                         \(tag_group) tag_text[tag_groups == tag_group]
                         )

            # combine into tibble ---
            tibble::tibble(
              acny_url = url_string,
              event_name = event_name_text,
              event_description = event_description_text,
              event_time = event_time_text,
              event_timestamp = event_timestamp,
              event_venues = list(venue_info),
              further_information = further_information_text,
              event_tags = list(tag_group_list)
              )
          })

        # Tabulate all events for this url
        purrr::list_rbind(events_data_per_event)

        })

  # Tabulate all events
  purrr::list_rbind(events_data_per_church)

}

#' Get tidy data on church contact details
#' @export
cny_scrape_contact <- function(cny_url){

  # make requests and parse html ---------------------------------------
  get_in_touch_req <- map_append_path(cny_url, "/get-in-touch/")

  get_in_touch_resp <-
    req_perform_parallel(events_req,
                         progress = "Obtaining Church Near You Data")

  get_in_touch_html <- purrr::map(events_resp, resp_body_html)

}


