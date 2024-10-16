#' Title
#'
#' @param level
#' @param nuts_code optional pattern match
#' @param year
#'
#' @export
all_nuts_codes <- function(level = 0L:4L, pattern = character(0L), year="2021"){

  qassert(pattern, "S*")
  qassert(level, "N+")

  if(is.numeric(year)) year <- as.character(year)
  year <- match.arg(year)

  ddir <- hs_data_dir("gisco", year, create_subdir=FALSE)
  if( !file.exists(file.path(ddir, "raw_codes.rqs")) ){
    stop("Maps data have not been downloaded for the given year:  see download_maps")
  }

  ## TODO: cache within package environment
  all_codes <- qread(file.path(ddir, "raw_codes.rqs"))

  all_codes |>
    select(NUTS="NUTS_ID", Level="LEVL_CODE", Label="NUTS_NAME") |>
    filter(Level %in% level) ->
    codes

  if(length(pattern)>0L){
    vapply(pattern, function(p){
      str_detect(all_codes[["NUTS"]], p)
    }, logical(nrow(all_codes))) |>
      apply(1,any) ->
      keep
    codes <- codes |> filter(keep)
  }

  codes
}
