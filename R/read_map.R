#' Internal `read_map` function
#' 
#' @param country_code ISO 3166-1 Country Code, e.g., `"DK"`
#' @param year Default to 2021, passed on to [all_nuts_codes()] and [hs_data_dir()].
#' 
read_map <- function(country_code, year){

  checkmate::qassert(country_code, "S1")
  checkmate::qassert(year, "S1")

  ## Verify that maps are available:
  ddir <- hs_data_dir("gisco", year, create_subdir=FALSE)
  if( !file.exists(file.path(ddir, "raw_codes.rqs")) || !file.exists(file.path(ddir, "raw_nuts.rqs")) || !file.exists(file.path(ddir, "raw_lau.rqs"))){
    stop("Maps data have not been downloaded for the given year:  see download_maps")
  }
  ## codes <- qs::qread(file.path(ddir, "raw_codes.rqs"))
  nuts <- qs::qread(file.path(ddir, "raw_nuts.rqs"))
  lau <- qs::qread(file.path(ddir, "raw_lau.rqs"))
  ## TODO: save within R package environment

  ## nuts_code should be a valid NUTS1 code:
  all_nuts0 <- all_nuts_codes(level=0L, year=year)[["NUTS"]]
  if(!country_code %in% all_nuts0) stop("Provided code '", country_code, "' is not a valid country code")

  ## filter to relevant NUTS0:
  nuts |>
    dplyr::filter(stringr::str_sub(NUTS_ID, 1L, 2L) == country_code) ->
    nuts
  lau |>
    dplyr::filter(stringr::str_sub(NUTS_ID, 1L, 2L) == country_code) |>
    dplyr::mutate(LEVL_CODE = 4, NUTS_NAME = LAU_NAME) ->
    lau

  dplyr::bind_rows(
    nuts,
    lau |> dplyr::select("NUTS_ID", "LEVL_CODE", "CNTR_CODE", "NUTS_NAME", "geometry")
  ) |>
    dplyr::select(NUTS="NUTS_ID", Level="LEVL_CODE", Label="NUTS_NAME", "geometry") ->
    all

  stopifnot(sum(all[["Level"]]==0L)==1L)

  invisible(all)
}
