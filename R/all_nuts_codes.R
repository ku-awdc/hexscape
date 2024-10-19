#' Retrieve all NUTS code
#'
#' @param level Levels are `0L`, `1L`, `2L` , `3L`, and `4L`.
#' @param nuts_code optional pattern match
#' @param year Default to 2021.
#'
#' @importFrom stringr str_detect
#'
#' @export
all_nuts_codes <- function(level = 0L:4L, pattern = character(0L), year="2021"){

  checkmate::qassert(pattern, "S*")
  checkmate::qassert(level, "N+")

  if(is.numeric(year)) year <- as.character(year)
  year <- match.arg(year)

  ddir <- hs_data_dir("gisco", year, create_subdir=FALSE)
  if( !file.exists(file.path(ddir, "raw_codes.rqs")) ){
    stop("Maps data have not been downloaded for the given year:  see download_maps")
  }

  ## TODO: cache within package environment
  all_codes <- qs::qread(file.path(ddir, "raw_codes.rqs"))

  all_codes |>
    dplyr::select(NUTS="NUTS_ID", Level="LEVL_CODE", Label="NUTS_NAME") |>
    dplyr::filter(Level %in% level) ->
    codes

  if(length(pattern)>0L){
    vapply(pattern, function(p){
      stringr::str_detect(codes[["NUTS"]], p)
    }, logical(nrow(codes))) |>
      apply(1,any) ->
      keep
    codes <- codes[keep,]
  }

  codes
}
