#' Title
#'
#' @param paths
#'
#' @importFrom httr2 request req_perform_sequential resp_body_string
#' @importFrom geojsonsf geojson_sf
#' @importFrom stringr str_c
#' @importFrom dplyr if_else
#'
#' @export
download_maps <- function(verbose = 1L, year = 2021, paths = NULL){

  ## Fixed for now:
  stopifnot(year==2021)

  ## If paths isn't given, impute defaults:
  builtin <- FALSE
  if(is.null(paths)){
    builtin <- TRUE
    paths <- c(nuts="https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_01M_2021_3035.geojson", lau="https://gisco-services.ec.europa.eu/distribution/v2/lau/geojson/LAU_RG_01M_2021_3035.geojson")

    paths <- c(nuts="https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_01M_2021_3035.geojson", lau="https://gisco-services.ec.europa.eu/distribution/v2/lau/geojson/LAU_RG_01M_2021_3035.geojson")
  }

  ## Verify input (if given):
  qassert(paths, "S2")
  assertNames(names(paths), permutation.of=c("nuts","lau"))

  ## Detect if each path is a file or url:
  is_file <- file.exists(paths)
  is_url <- c(FALSE,FALSE)
  if(!all(is_file)){
    is_url <- url.exists(paths)
  }
  if(!all(is_file | is_url)){
    if(builtin){
      stop("An unexpected error occured - one or more NUTS/LAU link was not found. Do you have an active internet connection?")
    }
    stop("One or more NUTS/LAU path is invalid")
  }
  if(any(is_file) && any(is_url)) stop("Mixing local files and remote urls is not currently supported")

  ## Load from file or from URL:
  sfcrs <- st_crs(3035)
  if(any(is_file)){
    nuts <- geojson_sf(paths[["nuts"]], input = sfcrs[["input"]], wkt = sfcrs[["wkt"]])
    lau <- geojson_sf(paths[["lau"]], input = sfcrs[["input"]], wkt = sfcrs[["wkt"]])
  }
  if(any(is_url)){
    if(verbose>0L) cat("Downloading files...\n")
    list(
      request(paths[["nuts"]]),
      request(paths[["lau"]])
      ) |>
      req_perform_sequential(progress=ifelse(verbose>0L, "Progress", FALSE)) ->
      rr

    nuts <- geojson_sf(resp_body_string(rr[[1L]]), input = sfcrs[["input"]], wkt = sfcrs[["wkt"]])
    lau <- geojson_sf(resp_body_string(rr[[2L]]), input = sfcrs[["input"]], wkt = sfcrs[["wkt"]])
  }

  ## Cache raw data:
  stopifnot(inherits(nuts, "sf"), inherits(nuts, "data.frame"), c("NUTS_ID","LEVL_CODE","CNTR_CODE","NUTS_NAME", "geometry") %in% names(nuts))
  nuts <- nuts |> select("NUTS_ID", "LEVL_CODE","CNTR_CODE","NUTS_NAME", "geometry")
  qsave(nuts, cache_path("raw", year, "nuts.rqs"))

  stopifnot(inherits(lau, "sf"), inherits(lau, "data.frame"), c("LAU_ID","CNTR_CODE","LAU_NAME", "POP_2021", "geometry") %in% names(lau))
  stopifnot(lau[["YEAR"]] == year)
  lau <- lau |> select("LAU_ID","CNTR_CODE","LAU_NAME", "POP_2021", "geometry")
  qsave(lau, cache_path("raw", year, "lau.rqs"))

  invisible(TRUE)
}
