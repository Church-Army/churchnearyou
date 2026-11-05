## HELPERS ##

## get cny url num from url

cny_url_num <- function(url){
  out <- stringr::str_extract(url, "\\d+")
  if(any(is.na(out))) stop("Missing/invalid URLs provided")
  out
}

map_request_make <- function(url, path){

  num <- cny_url_num(url)

  purrr::map(num,
             \(x){
               cny_request(x) |>
                 httr2::req_url_path_append(path)
             })
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

parallel_html_get <- function(req_list){

  httr2::req_perform_parallel(
    req_list,
    progress = "Obtaining Church Near You Data") |>

    purrr::map(httr2::resp_body_html)

}

cny_page_url <- function(req){

  stringr::str_extract(req$url, "https://www.achurchnearyou.com/church/\\d+/")

}

get_text <- function(x) rvest::html_text(x, trim = TRUE)

## EXPORTS ##

#' Get tidy(ish) data on Church Events
#' @export
cny_scrape_events <- function(cny_url){

  ## Perform requests and parse html data -------------------------------------------------
  requests <- map_request_make(cny_url, "service-and-events/events-all/")

  events_html <- parallel_html_get(requests)

  ## Extract 'event cards' for each cny url ------------------------------------------------
  events_data_per_church <-

    purrr::map2(

      events_html, events_req,

      \(result, req){

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
              acny_url = cny_page_url(req),
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
  contact_req <- map_request_make(cny_url, "/get-in-touch/")

  contact_html <- parallel_html_get(contact_req)

  contact_data_by_church <-
    purrr::map2(
      contact_html, contact_req,
      \(html, req){

        all_dests <- rvest::html_elements(html, "a") |>
          rvest::html_attr("href")

        is_phone_number <- function(x){
          stringr::str_detect(x, "tel\\:")
        }

        phone <-
          all_dests[stringr::str_detect(all_dests, "tel\\:[0-9 \\+\\-\\(\\)(ext|extension)]")] |>
          stringr::str_remove("tel\\:")

        if(length(phone) == 0) phone <- NA_character_

        phone <- phone[1]



        external_links <-
          all_dests[
            stringr::str_detect(
              all_dests,
              "^/|tel\\:|churchofengland|archbishopof(canterbury|york)|https://www.dev.ngo/|media.acny.uk|#|achurchnearyou.com",
              negate = TRUE
              )
          ]

        socials_reprex <- stringr::str_c(socials, collapse = "|")

        website <-
          external_links[str_detect(external_links, socials_reprex, negate = TRUE)]

        social_links <-
          purrr::map(socials,
              \(x){
                links <- external_links[str_detect(external_links, x)]
                links[1]
              })

        socials_tibble <- tibble::as_tibble(social_links)

        tibble::tibble(
          acny_url = cny_page_url(req),
          website = website[1], phone = phone) |>
          cbind(socials_tibble)

      })

  purrr::list_rbind(contact_data_by_church)

}

socials <- c(
  facebook = "facebook.com",
  youtube  = "youtube.com",
  instagram = "instagram.com",
  x_twitter = "x.com|twitter.com",
  tiktok = "tiktok.com"
)
