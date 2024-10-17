#' Download the NUTS and LAU datasets
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
#' @importFrom qs qsave qread
#' @importFrom tibble as_tibble
#' @importFrom sf st_intersection st_intersects st_centroid st_contains st_crs
#' @importfrom checkmate qassert assertNames
#' 
#' @export
download_maps <- function(year = "2021", verbose = 1L, paths = NULL){

  ## Fixed to 2021 for now:
  if(is.numeric(year)) year <- as.character(year)
  year <- match.arg(year)

  ## Check to see if files already exist:
  ddir <- hs_data_dir("gisco", year, create_subdir=TRUE)
  if( file.exists(file.path(ddir, "raw_codes.rqs")) ){
    stop("The maps files have already been downloaded - to re-download,\n  delete raw_codes.rqs from the path given by:\n  hs_data_dir('gisco', ", year, ")")
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
    if(verbose>0L) cat("Download complete.\n")

    nuts <- geojson_sf(resp_body_string(rr[[1L]]), input = sfcrs[["input"]], wkt = sfcrs[["wkt"]])
    lau <- geojson_sf(resp_body_string(rr[[2L]]), input = sfcrs[["input"]], wkt = sfcrs[["wkt"]])

    #save(nuts, lau, file="nuts_lau_temp.rda")
  }


  stopifnot(inherits(nuts, "sf"), inherits(nuts, "data.frame"), c("NUTS_ID","LEVL_CODE","CNTR_CODE","NUTS_NAME", "geometry") %in% names(nuts))
  stopifnot(inherits(lau, "sf"), inherits(lau, "data.frame"), c("LAU_ID","CNTR_CODE","LAU_NAME", str_c("POP_", year), "geometry") %in% names(lau))

  ## Add corresponding nuts3 to lau:
  laur <- nrow(lau)
  lau |>
    group_by(CNTR_CODE) |>
    group_split() |>
    map(function(ll){
      nuts |>
        filter(LEVL_CODE==3, CNTR_CODE==ll[["CNTR_CODE"]][1L]) ->
        nuts3

      st_contains(
        nuts3 |> pull(geometry),
        ll |> pull(geometry) |> st_centroid(of_largest_polygon=TRUE),
        sparse=FALSE
      ) -> nuts3_contains_lau
      nc <- apply(nuts3_contains_lau,2L,which,simplify=FALSE)
      stopifnot(sapply(nc,length) %in% c(0,1))
      for(i in which(sapply(nc,length)==0L)){
        st_intersects(
          nuts3 |> pull(geometry),
          ll |> slice(i) |> pull(geometry),
          sparse=FALSE
        )[,1L] ->
          ii

        if(sum(ii)==0L){
          stop("No matches in NUTS for LAU ", lau |> slice(i) |> pull(LAU_ID))
        }else if(sum(ii)==1L){
          nc[[i]] <- which(ii)
        }else if(sum(ii)>1L){
          st_intersection(
            nuts3[ii,] |> pull(geometry),
            ll |> slice(i) |> pull(geometry)
          ) |>
            st_area() ->
            areas
          if(all(areas == max(areas))) stop("Area-based tie in NUTS for LAU ", lau |> slice(i) |> pull(LAU_ID))
          nc[[i]] <- which(ii)[which.max(areas)]
        }else{
          stop("Logic error matching LAU to NUTS")
        }
      }
      stopifnot(sapply(nc,length)==1L)
      ll |>
        ungroup() |>
        mutate(NUTS3 = nuts3[["NUTS_ID"]][sapply(nc, identity)]) |>
        mutate(NUTS_ID = str_c(.data$NUTS3, "_", .data$LAU_ID)) |>
        select("NUTS_ID", everything())
    }, .progress=verbose>1L) |>
    bind_rows() ->
    lau
  stopifnot(nrow(lau)==laur)

  if(verbose>0L) cat("Saving files...\n")

  ## Cache nuts:
  nuts <- nuts |> select("NUTS_ID", "LEVL_CODE","CNTR_CODE","NUTS_NAME", "geometry")
  fp <- file.path(ddir, "raw_nuts.rqs")
  if(file.exists(fp)) file.remove(fp)
  qsave(nuts, file=fp)

  ## Cache lau:
  ## stopifnot(lau[["YEAR"]] == year)
  lau <- lau |> select("NUTS_ID", "CNTR_CODE","LAU_NAME","YEAR",POP=str_c("POP_",year),"geometry")
  fp <- file.path(ddir, "raw_lau.rqs")
  if(file.exists(fp)) file.remove(fp)
  qsave(lau, file=fp)

  ## Cache nuts codes:
  bind_rows(
    nuts |> as_tibble() |> select("NUTS_ID", "NUTS_NAME", "CNTR_CODE", "LEVL_CODE"),
    lau |> as_tibble() |> mutate(LEVL_CODE=4L) |> select("NUTS_ID", NUTS_NAME="LAU_NAME", "CNTR_CODE", "LEVL_CODE")
  ) |>
    arrange(.data$LEVL_CODE, .data$CNTR_CODE, .data$NUTS_ID) ->
    codes
  fp <- file.path(ddir, "raw_codes.rqs")
  if(file.exists(fp)) file.remove(fp)
  qsave(codes, file=fp)


  if(verbose>0L) cat("Map files downloaded and saved successfully.\n")

  invisible(TRUE)
}
