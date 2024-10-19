#' Download CORINE data for one or more NUTS 1 code
#'
#' @param year Default to 2021.
#' @param verbose
#' @param paths Named character vector, with names `nuts` and ``lau`
#'
#' @importFrom httr2 request req_perform_sequential resp_body_string
#' @importFrom geojsonsf geojson_sf
#' @importFrom stringr str_c str_detect
#' @importFrom dplyr arrange slice pull if_else bind_rows mutate filter group_split group_by ungroup select
#' @importFrom purrr map
#' @importFrom rlang .data
#' @importFrom qs qsave qread qdeserialize
#' @importFrom tibble as_tibble
#' @importFrom sf st_intersection st_intersects st_centroid st_contains st_crs
#' @importfrom checkmate qassert assertNames
#'
#' @export
download_maps <- function(nuts_codes, year = "2018", verbose = 1L){

  qassert(nuts_codes, "S+")

  ## Shortcut to allow all NUTS levels to be specified:
  lapply(nuts_codes, function(x){
    nc <- all_nuts_codes(level=1L, pattern = str_c("^", str_sub(x, 1L, pmin(3L, str_length(x)))))
    if(nrow(nc)==0L) stop(x, " does not match a valid NUTS code")
    nc
  }) |>
    bind_rows() |>
    pull(NUTS) |>
    unique() ->
    nuts_codes
  stopifnot(nuts_codes %in% all_nuts_codes(level=1L)[["NUTS"]])

  if(is.numeric(year)) year <- as.character(year)
  year <- match.arg(year)


  ## Check to see if files already exist, and remove them:
  ddir <- hs_data_dir("corine", year, create_subdir=TRUE)
  exists <- file.exists(file.path(ddir, year, str_c(nuts_codes, ".rqs")))
  nuts_codes <- nuts_codes[!exists]
  if(length(nuts_codes)==0L){
    stop("All specified CORINE files have already been downloaded - to re-download, delete one or more files in the following directory before re-running download_corine():  ", ddir)
  }

  ## Download the info file:
  if(verbose>0L) cat("Downloading info file...\n")
  ss <- try({
    request("https://www.costmodds.org/rsc/hexscape/corine/info.rqs") |>
      req_perform() |>
      resp_body_raw() |>
      qdeserialize() |>
      filter(NUTS1 %in% nuts_codes, CorineYear == year) ->
      sinfo
  })
  if(inherits(ss, "try-error")) stop("There was a problem downloading the info file - no internet connection?")

  if(any(sinfo[["Link"]] == "")){
    notav <- which(sinfo[["Link"]] == "")
    stop("CORINE data for the following NUTS code(s) is not available:\n  ", str_c(sinfo[["NUTS1"]][notav], collapse=", "), "\n  ", str_c(unique(sinfo[["Comment"]][notav]), collapse="\n  "))
  }

  if (verbose > 0L)
    cat("Downloading ", nrow(sinfo), " CORINE file(s)...\n", sep = "")
  ss <- try({
    sinfo |>
      pull(.data$Link) |>
      lapply(request) |>
      req_perform_sequential(progress = ifelse(verbose > 0L, "Progress", FALSE)) ->
      rr
    stopifnot(length(rr) == nrow(sinfo))
  })
  if (inherits(ss, "try-error"))
    stop("There was a problem downloading CORINE files - no internet connection?")

  ss <- try({
    rr |>
      lapply(function(x)
        qdeserialize(resp_body_raw(x))) ->
      all_corine_raw
    stopifnot(sinfo[["NUTS1"]] == sapply(all_corine_raw, \(x) attr(x, "nuts1")))
  })
  if (inherits(ss, "try-error"))
    stop(
      "There was a problem de-serlialising CORINE files - the files may have been corrupted during download. Try again, or contact the package developer for support."
    )

  lapply(all_corine_raw, function(x){
    nuts1 <- attr(all_corine_raw, "nuts1")
    pth <- file.path(ddir, str_c(nuts1, ".rqs"))
    qsave(x, pth, preset="archive")
  })

  if(verbose>0L) cat("CORINE data files downloaded and saved successfully.\n")

  invisible(TRUE)
}
