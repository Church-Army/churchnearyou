#' Get tidy(ish) data on Church Events
#' @export
cny_scrape_events <- function(cny_url){

  events_req <-
    purrr::map(cny_url,
               \(url){
                 cny_request(url) |>
                   httr2::req_url_path_append("service-and-events/events-all/")
                   })

  events_resp <- req_perform_parallel(events_req, progress = "Obtaining Church Near You Data")

  events_html <- purrr::map(events_resp, resp_body_html)

  events_data <-
    purrr::map2(
      events_html, cny_url,
      \(x, y){

        events <- rvest::html_elements(x, ".card__summary")

        events_data <-
          purrr::map(events, \(x){

          card_title <- rvest::html_elements(x, ".card__title")
          card_meta <- rvest::html_elements(x, ".card__meta")

          event_name <- rvest::html_text(card_title, trim = TRUE)

          venue <- rvest::html_elements(card_meta, ".meta__venue")

          venue_names <- rvest::html_elements(venue, ".venue") |> rvest::html_text(trim = TRUE)

          venue_addresses <-
            rvest::html_elements(venue, ".address") |>
            rvest::html_text(trim = TRUE) |>
            stringr::str_replace_all("\r\n", ", ")

          venue_info <- tibble::tibble(venue_name = venue_names, venue_address = venue_addresses)

          event_description <-
            rvest::html_elements(card_meta, ".meta__description.card__event-description") |>
            rvest::html_text(trim = TRUE)

          descriptions <- rvest::html_elements(card_meta, ".meta__description")

          event_time <- descriptions[1]

          event_timestamp <-
            rvest::html_elements(event_time, "time") |>
            rvest::html_attr("datetime")

          event_time <- rvest::html_text(event_time, trim = TRUE)

          further_information <-
            descriptions[-(1:2)] |> rvest::html_text(trim = TRUE)

          if(length(further_information) == 0) further_information <- NA_character_


          tags <-
            rvest::html_elements(card_meta, ".tag.tag--event")

          tag_classes <-
            rvest::html_attr(tags, "class")

          tag_groups <- stringr::str_extract(tag_classes, "[a-z\\-]+$")

          tag_text <- rvest::html_text(tags, trim = TRUE)

          tag_group_list <- as.list(sort(unique(tag_groups)))
          names(tag_group_list) <- tag_group_list

          tag_group_list <-

            purrr::map(tag_group_list,
              \(x){
                tag_text[tag_groups == x]
              })

          tibble::tibble(
            acny_url = y,
            event_name = event_name,
            event_description = event_description,
            event_time = event_time,
            event_timestamp = event_timestamp,
            event_venues = list(venue_info),
            further_information = further_information,
            event_tags = list(tag_group_list)
          )

          })

        purrr::list_rbind(events_data)

        })

  purrr::list_rbind(events_data)

}

#' Get tidy data on church contact details
#' @export
#'
cny_scrape_contact(cny_url){

  get_in_touch_url <- paste0(cny_url, "/get-in-touch/")

}

cny_request <- function(url){

    httr2::request(url) |>
    httr2::req_throttle(capacity = 120, fill_time_s = 60) |>
    httr2::req_user_agent("R Package 'churchnearyou' (in development) https://github.com/church-army/churchnearyou")
}
