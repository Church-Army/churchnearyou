where_in <- function(x, field_name, quotes = FALSE){
  if(!is.null(x)){

    if(quotes) x <- paste0("'", x, "'")

    out <- paste0(x, collapse = ", ")
    out <- paste0(field_name, " IN (", out, ")")
  } else out <- NULL
  out
}

create_where_string <-
  function(diocese_name = NULL,
           diocese_number = NULL,
           church_code = NULL,
           parish_id = NULL,
           where = ""){

    diocese_name <- where_in(diocese_name, "DioNa", quotes = TRUE)
    diocese_number <- where_in(diocese_number, "DioID")
    church_code <- where_in(church_code, "Church_Cod")
    parish_id <- where_in(parish_id)


    out <- paste0(diocese_name, diocese_number, church_code, parish_id,
           where,
           collapse = " AND ")



    if(out == "") out = "1=1"

    out

  }

#'
#'
#' @export
cny_churches <- function(diocese_name = NULL,
                         diocese_number = NULL,
                         church_code = NULL,
                         parish_id = NULL,
                         where = ""){

  where_string <-
    create_where_string(
    diocese_name = diocese_name,
    diocese_number = diocese_number,
    church_code = church_code,
    parish_id = parish_id,
    where = where
  )

  result <- churches("*", where = where_string)

  old_names <- c("Church_Nam", "Church_Cod", "ACNY_url", "ACNY_ID", "ParLocNa", "ParishID", "BenefNa", "BenefID",
                 "DeanNa", "DeanID", "ArchdNa", "ArchdID", "DioNa", "DioID", "Longitude", "Latitude")

  new_names <- c("church_name", "church_code", "acny_url", "acny_id", "parish_local_name", "parish_id",
                 "benefice_name", "benefice_id", "deanery_name", "deanery_id", "archdeaconry_name", "archdeaconry_id", "diocese_name", "diocese_id",
                 "longitude", "latitude")

  if(ncol(result)==0){
    result <- vector("list", length(old_names))
    names(result) <- old_names
  }

  result <- tibble::as_tibble(result)

  result <- result[old_names]

  names(result) <- new_names

  result

}

churches <- function(select, where){

  message("Obtaining church location data from July 2026. Any reproduction of this data should include the notice '© Church of England, Data Services, 2026.'")

  church_layer <- "https://services5.arcgis.com/KDRjxGRQDVgVtFTS/arcgis/rest/services/Churches_July2026/FeatureServer/0/query"

  result_tibble <- empty_result_tibble
  resultOffset <- 0

  request <-
    httr2::request(church_layer) |>
    httr2::req_user_agent("R package churchnearyou - development version") |>
    httr2::req_throttle(capacity = 20, fill_time_s = 60)

  repeat{

    req_iteration <-
      httr2::req_url_query(request,
                           where = where, outFields = select,
                           resultOffset = resultOffset,
                           returnGeometry = "false", f = "json")

    latest_tibble <-
      httr2::req_perform(req_iteration) |>
      httr2::resp_body_json() |>
      purrr::pluck("features") |>
      purrr::map("attributes") |>
      purrr::list_transpose() |>
      tibble::as_tibble()

    ## Stop repeating if the tibble was empty
    if(nrow(latest_tibble) == 0) break

    ## Row-bind latest tibble with previous results
    result_tibble <- rbind(result_tibble, latest_tibble)

    ## Increment resultOffset to get next 200 rows
    resultOffset <- resultOffset + 2000
  }

  result_tibble
}

empty_result_tibble <- structure(
  list(FID = integer(0), Church_Nam = character(0), ParLegNa = character(0),
       ACNY_url = character(0), Church_Her = character(0), Church_Cod = integer(0),
       ParLocNa = character(0), ParishID = integer(0), BenefNa = character(0),
       BenefID = character(0), DeanNa = character(0), DeanID = integer(0),
       ArchdNa = character(0), ArchdID = integer(0), DioNa = character(0),
       DioID = integer(0), LBGrade = character(0), Longitude = numeric(0),
       Latitude = numeric(0), ACNY_ID = integer(0)), row.names = c(NA,0L),
  class = c("tbl_df", "tbl", "data.frame"))
