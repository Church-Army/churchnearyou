#' @export
cny_scrape <- function(cny_url){

  events_url <- paste0(cny_url, "/service-and-events/events-all/")

  result <- purrr::map(events_url,

                       \(x){
                         httr::with_config(
                           {rvest::read_html(x)},
                           config = httr::config(user_agent = "https://github.com/church-army/churchnearyou/")
                           )},

                       .progress = "Obtaining Church Near You Data")


  events_data <-
    purrr::map2(
      result, cny_url,
      \(x, y){

        events <- html_elements(x, ".card__summary")

        events_data <-
          map(events, \(x){

          card_title <- html_elements(x, ".card__title")
          card_meta <- html_elements(x, ".card__meta")

          event_name <- html_text(card_title, trim = TRUE)

          venue <- html_elements(card_meta, ".meta__venue")

          venue_names <- html_elements(venue, ".venue") |> html_text(trim = TRUE)

          venue_addresses <-
            html_elements(venue, ".address") |>
            html_text(trim = TRUE) |>
            stringr::str_replace_all("\r\n", ", ")

          venue_info <- tibble::tibble(venue_name = venue_names, venue_address = venue_addresses)

          event_description <-
            html_elements(card_meta, ".meta__description.card__event-description") |>
            html_text(trim = TRUE)

          descriptions <- html_elements(card_meta, ".meta__description")

          event_time <- descriptions[1]

          event_timestamp <-
            html_elements(event_time, "time") |>
            html_attr("datetime")

          event_time <- html_text(event_time, trim = TRUE)

          further_information <-
            descriptions[-(1:2)] |> html_text(trim = TRUE)

          if(length(further_information) == 0) further_information <- NA_character_


          tags <-
            html_elements(card_meta, ".tag.tag--event")

          tag_classes <-
            html_attr(tags, "class")

          tag_groups <- stringr::str_extract(tag_classes, "[a-z\\-]+$")

          tag_text <- html_text(tags, trim = TRUE)

          tag_group_list <- as.list(sort(unique(tag_groups)))
          names(tag_group_list) <- tag_group_list

          tag_group_list <-

            map(tag_group_list,
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

