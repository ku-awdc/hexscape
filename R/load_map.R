#' Title
#'
#' @param country_code
#' @param nuts_year
#' @param verbose
#'
#' @importFrom eurostat get_eurostat_geospatial
#' @importFrom purrr quietly
#'
#' @export
load_map <- function(country_code, nuts_year=2016, verbose=1L){

  stopifnot(length(country_code)==1)
  refresh <- FALSE

  valid <- str_detect(country_code, "^[[:ALPHA:]][[:ALPHA:]]") & str_length(country_code) %in% c(2)
  if(any(!valid)) stop("The following country codes are in an invalid format: ", str_c(country_code[!valid], collapse=", "))

  storage_folder <- hexscape_getOption("storage_folder")

  savename <- file.path(storage_folder, "eurostat_cache", str_c("map_", country_code, ".rqs"))
  if(!refresh && file.exists(savename)){
    if(verbose > 1L) cat("Returning cached eurostat sf data for ", country_code, "...\n", sep="")
    return(qread(savename))
  }

  if(verbose > 0L) cat("Extracting eurostat sf data for ", country_code, "...\n", sep="")

  qf <- quietly(get_eurostat_geospatial)
  qf(output_class = "sf", resolution = "01", nuts_level = '3',
                                 year = nuts_year, crs = "4326",
                                 cache_dir = file.path(storage_folder, "eurostat_cache"),
                                 make_valid = TRUE)[["result"]] |>
    st_make_valid() ->
    map

  map <- map %>%
    filter(CNTR_CODE %in% country_code) %>%
    select(CNTR_CODE, NUTS_ID, NUTS_NAME, geometry)

  attr(map, "nuts_year") <- nuts_year
  attr(map, "version") <- hexscape_version()

  if(nrow(map)==0L) stop(country_code, " does not seem to be a valid country code")

  qsave(map, savename)

  return(map)

}
