#' Extract a map for given country or NUTS area code
#'
#' @param nuts_code
#' @param nuts_label
#' @param verbose
#'
#' @importFrom eurostat get_eurostat_geospatial
#' @importFrom purrr quietly
#'
#' @export
load_map <- function(nuts_code, nuts_label, verbose=1L){

  if(!missing(nuts_label)){
    stop("nuts_label is not yet implemented")
    #str_detect()
  }

  nuts_year <- 2016

  union <- FALSE
  if(!isFALSE(union)){
    stop("union is not yet implemented")
    # Can be FALSE (no union, labels at specified arguments), TRUE (complete union no labels), or 0/1/2/3 (union at specified NUTS level, labels??)
    #str_detect()
  }

  ## nuts_code can be a country or NUTS0/1/2/3 code:
  invalid <- which(!nuts_code %in% nuts_codes[["NUTS"]])
  if(length(invalid)>0L){
    stop("The following NUTS codes supplied are invalid: ", str_c(nuts_code[invalid], collapse=", "))
  }

  nuts_codes |>
    filter(NUTS %in% nuts_code) ->
  nuts_using

  nuts_using |>
    distinct(Code) |>
    pull(Code) ->
  country_code

  if(length(country_code)>1L) stop("Fetching maps for multiple countries is not yet implemented")
  stopifnot(length(country_code)==1)
  refresh <- FALSE

  storage_folder <- hexscape_getOption("storage_folder")

  savename <- file.path(storage_folder, "eurostat_cache", str_c("map_", country_code, ".rqs"))

  if(!refresh && file.exists(savename)){
    if(verbose > 1L) cat("Returning cached sf data for ", country_code, "...\n", sep="")
    map <- qread(savename)
    if(length(attr(map, "version"))==1L && attr(map, "version") >= package_version("0.4.0")){
      cache_ok <- TRUE
    }else{
      cache_ok <- FALSE
      if(verbose > 0L) cat("Re-extracting sf data for ", country_code, " (required for new HexScape version)...\n", sep="")
    }
  }else{
    if(verbose > 0L) cat("Extracting sf data for ", country_code, "...\n", sep="")
    cache_ok <- FALSE
  }

  if(!cache_ok){

    qf <- quietly(get_eurostat_geospatial)
    qf(output_class = "sf", resolution = "01", nuts_level = "all",
       year = nuts_year, crs = "4326",
       cache_dir = file.path(storage_folder, "eurostat_cache"),
       make_valid = TRUE)[["result"]] |>
      st_make_valid() |>
      filter(CNTR_CODE %in% country_code) |>
      select(CNTR_CODE, NUTS_ID, NUTS_NAME, geometry) ->
      map

    attr(map, "nuts_year") <- nuts_year
    attr(map, "version") <- hexscape_version()

    if(nrow(map)==0L) stop(country_code, " does not seem to be a valid country code")

    qsave(map, savename)
  }

  map |>
    select(Code = CNTR_CODE, NUTS=NUTS_ID, geometry) |>
    inner_join(nuts_using, by=c("Code","NUTS")) |>
    select(Code, Country, NUTS, Level, Label, geometry) |>
    st_make_valid() ->
  map

  if(nrow(map)==0L) stop("An unknown error occured fetching a map for the specified area")

  if(union){
    stop("TODO")
  }

  attr(map, "nuts_year") <- nuts_year
  attr(map, "version") <- hexscape_version()

  return(map)

}
