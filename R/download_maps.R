#' Title
#'
#' @param year
#' @param verbose
#' @param paths
#'
#' @importFrom httr2 request req_perform_sequential resp_body_string
#' @importFrom geojsonsf geojson_sf
#' @importFrom stringr str_c str_detect
#' @importFrom dplyr if_else
#'
#' @export
download_maps <- function(year = "2021", verbose = 1L, paths = NULL){

  ## Fixed for now:
  stopifnot(year==2021)

  ## Check to see if files already exist:
  cdir <- cache_dir("raw", year, create_subdir=TRUE)
  if( file.exists(file.path(cdir, "nuts.rqs")) && file.exists(file.path(cdir, "lau.rqs")) ){
    stop("The requested nuts and lau file already exist - to re-download, delete one or both manually (see ?cache_dir)")
  }

  ## If paths isn't given, impute defaults:
  builtin <- FALSE
  if(is.null(paths)){
    builtin <- TRUE
    paths <- c(nuts="https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_01M_2021_3035.geojson", lau="https://gisco-services.ec.europa.eu/distribution/v2/lau/geojson/LAU_RG_01M_2021_3035.geojson")
  }

  ## Verify input (if given):
  qassert(paths, "S2")
  assertNames(names(paths), permutation.of=c("nuts","lau"))

  is_file <- file.exists(paths)
  is_url <- str_detect(paths, "^http[s]?://")
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
    cat("Download complete.\n")

    nuts <- geojson_sf(resp_body_string(rr[[1L]]), input = sfcrs[["input"]], wkt = sfcrs[["wkt"]])
    lau <- geojson_sf(resp_body_string(rr[[2L]]), input = sfcrs[["input"]], wkt = sfcrs[["wkt"]])
  }

  browser()

  ## Cache raw data:
  stopifnot(inherits(nuts, "sf"), inherits(nuts, "data.frame"), c("NUTS_ID","LEVL_CODE","CNTR_CODE","NUTS_NAME", "geometry") %in% names(nuts))
  nuts <- nuts |> select("NUTS_ID", "LEVL_CODE","CNTR_CODE","NUTS_NAME", "geometry")

  fp <- file.path(cdir, "nuts.rqs")
  if(file.exists(fp)) file.remove(fp)
  qsave(nuts, file=fp)

  stopifnot(inherits(lau, "sf"), inherits(lau, "data.frame"), c("LAU_ID","CNTR_CODE","LAU_NAME", "POP_2021", "geometry") %in% names(lau))
  ## stopifnot(lau[["YEAR"]] == year)
  lau <- lau |> select("LAU_ID","CNTR_CODE","LAU_NAME","YEAR",POP=str_c("POP_",year),"geometry")

  fp <- file.path(cdir, "lau.rqs")
  if(file.exists(fP)) file.remove(fp)
  qsave(lau, file=fp)

  cat("Map files downloaded and cached successfully.\n")

  invisible(TRUE)
}
