#'
#'
#' @export
cny_churches <- function(){

  result <-
    churches("*", where = "1=1")

  result <- tibble::as_tibble(result)
  result <- result[!names(result) %in% "geometry"]

  result <- result[c("Church_Nam", "Church_Cod", "ACNY_url", "ACNY_ID", "ParLocNa", "ParishID", "BenefNa", "BenefID",
                     "DeanNa", "DeanID", "ArchdNa", "ArchdID", "DioNa", "DioID", "Longitude", "Latitude")]

  names(result) <- c("church_name", "church_code", "acny_url", "acny_id", "parish_local_name", "parish_id",
                     "benefice_name", "benefice_id", "deanery_name", "deanery_id", "archdeaconry_name", "archdeaconry_id", "diocese_name", "diocese_id",
                     "longitude", "latitude")

  result

}

churches <- function(select, where){

  message("Obtaining church location data from July 2026. Any reproduction of this data should include the notice '© Church of England, Data Services, 2026.'")

  church_layer <- "https://services5.arcgis.com/KDRjxGRQDVgVtFTS/arcgis/rest/services/Churches_July2026/FeatureServer/0/query"

  request <-
    httr2::request(church_layer) |>
    httr2::req_user_agent("R package churcnearyou - development version") |>
    httr2::req_url_query(where = where, outFields = select,
                         returnGeometry = "false", f = "json")

  httr2::req_perform(request) |>
    httr2::resp_body_json() |>
    purrr::pluck("features") |>
    purrr::map("attributes") |>
    purrr::list_transpose() |>
    tibble::as_tibble()
  }
