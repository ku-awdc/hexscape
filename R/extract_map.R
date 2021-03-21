#' Title
#'
#' @param country_code
#' @param nuts_year
#' @param refresh
#'
#' @importFrom eurostat get_eurostat_geospatial
#' @export
extract_map <- function(country_code, nuts_year=2016, refresh=FALSE, verbose=1L){

  stopifnot(length(country_code)==1)

  storage_folder <- hexscape_getOption("storage_folder")

  savename <- file.path(storage_folder, "eurostat_cache", str_c("map_", country_code, ".rds"))
  if(!refresh && file.exists(savename)){
    if(verbose > 1L) cat("Returning cached eurostat sf data for ", country_code, "...\n", sep="")
    return(readRDS(savename))
  }

  if(verbose > 0L) cat("Extracting eurostat sf data for ", country_code, "...\n", sep="")
  map <- get_eurostat_geospatial(output_class = "sf", resolution = "01", nuts_level = '3',
                                 year = nuts_year, crs = "3035",
                                 # Note: this projection avoids warnings from sf later on
                                 # Note: cache_dir seems to be ignored??
                                 cache_dir = file.path(storage_folder, "eurostat_cache")) %>%
    filter(CNTR_CODE %in% country_code) %>%
    select(CNTR_CODE, NUTS_ID, NUTS_NAME, geometry)

  stopifnot(nrow(map)>0)

  saveRDS(map, savename, compress=TRUE)

  return(map)

}
