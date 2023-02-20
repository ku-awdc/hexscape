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
    if(length(attr(map, "version"))==1L && attr(map, "version") >= package_version("0.4.3")){
      cache_ok <- TRUE
    }else{
      cache_ok <- FALSE
      if(verbose > 0L) cat("Re-extracting sf data for ", country_code, " (required for new hexscape version)...\n", sep="")
    }
  }else{
    if(verbose > 0L) cat("Extracting sf data for ", country_code, "...\n", sep="")
    cache_ok <- FALSE
  }

  if(!cache_ok){

    ## Note: anything other than nuts_level==3 produces lower res maps
    if(is.null(hexscape_env$eurostat_map)){

      ## Old eurostat code - not using as resolution is not always as advertised
      if(FALSE){
        ## Note: changed this as well
        nuts_year <- 2016
        if(verbose > 0L) cat("Extracting eurostat geospatial data...\n", sep="")
        qf <- quietly(get_eurostat_geospatial)
        qf(output_class = "sf", resolution = "01", nuts_level = "3",
           year = nuts_year, crs = "4326",
           cache_dir = file.path(storage_folder, "eurostat_cache"),
           make_valid = TRUE)[["result"]] |>
          st_make_valid() ->
          map
      }

      ## New code reading from file:
      mapfile <- file.path(storage_folder, "raw_data", "NUTS_RG_01M_2021_4326.shp")
      if(!dir.exists(mapfile)){
        stop("The eurostat map file directory (NUTS_RG_01M_2021_4326.shp) was not found - see the help file for instructions")
      }
      tt <- capture.output(map <- st_read(mapfile))

      attr(map, "eurostat_path") <- mapfile
      hexscape_env$eurostat_map <- map
    }else{
      map <- hexscape_env$eurostat_map
    }

    eurostat_path <- attr(map, "eurostat_path")
    map <- map |>
      filter(CNTR_CODE %in% country_code) |>
      select(CNTR_CODE, NUTS_ID, NUTS_NAME, geometry)

    ## Then re-create all higher NUTS divisions by union:
    nuts_codes |>
      filter(Code==country_code) ->
    all_nuts

    stopifnot(all_nuts |> filter(Level==3) |> pull(NUTS) %in% map$NUTS_ID)

    # NUTS3 we already have:
    all_nuts |>
      filter(Level==3) |>
      select(NUTS_ID = NUTS, everything()) |>
      inner_join(
        map,
        by="NUTS_ID"
      ) |>
      mutate(geometry = st_make_valid(geometry), centroid = st_centroid(geometry, of_largest_polygon=TRUE)) |>
      select(NUTS=NUTS_ID, Code, Country, Level, Label, centroid, geometry) |>
      st_as_sf(sf_column_name = "geometry", crs=st_crs(map)) ->
    nuts3
    # ggplot(nuts3) + geom_sf() + geom_sf_text(aes(geometry=centroid, label=NUTS))

    if(!all(st_is_valid(nuts3$geometry))) stop("Invalid geometries created by eurostat")
    if(any(st_is_empty(nuts3$geometry))) stop("Missing geometries created by eurostat")

    # NUTS2:
    all_nuts |>
      ## For testing:
      # bind_rows(tibble(Code="DK",Country="Denmark",Level=2,NUTS="DKY!",Label="")) |>
      filter(Level==2) |>
      left_join(
        nuts3 |>
          mutate(NUTS = str_sub(NUTS, 1L, 4L)) |>
          group_by(NUTS) |>
          summarise(geometry = st_union(geometry) |> st_make_valid(),
                    .groups="drop"),
        by="NUTS") |>
      mutate(geometry = st_make_valid(geometry), centroid = st_centroid(geometry, of_largest_polygon=TRUE)) |>
      select(NUTS, Code, Country, Level, Label, centroid, geometry) |>
      st_as_sf(sf_column_name = "geometry", crs=st_crs(map)) ->
      nuts2

    if(!all(st_is_valid(nuts2$geometry))) stop("Invalid geometries created by union to NUTS2")
    if(any(st_is_empty(nuts2$geometry))) stop("Missing geometries after union to NUTS2")
    # ggplot(nuts2) + geom_sf() + geom_sf_text(aes(geometry=centroid, label=NUTS))

    # NUTS1:
    all_nuts |>
      ## For testing:
      # bind_rows(tibble(Code="DK",Country="Denmark",Level=2,NUTS="DKY!",Label="")) |>
      filter(Level==1) |>
      left_join(
        nuts3 |>
          mutate(NUTS = str_sub(NUTS, 1L, 3L)) |>
          group_by(NUTS) |>
          summarise(geometry = st_union(geometry) |> st_make_valid(),
                    .groups="drop"),
        by="NUTS") |>
      mutate(geometry = st_make_valid(geometry), centroid = st_centroid(geometry, of_largest_polygon=TRUE)) |>
      select(NUTS, Code, Country, Level, Label, centroid, geometry) |>
      st_as_sf(sf_column_name = "geometry", crs=st_crs(map)) ->
      nuts1

    if(!all(st_is_valid(nuts1$geometry))) stop("Invalid geometries created by union to NUTS1")
    if(any(st_is_empty(nuts1$geometry))) stop("Missing geometries after union to NUTS1")
    # ggplot(nuts1) + geom_sf() + geom_sf_text(aes(geometry=centroid, label=NUTS))

    # NUTS0:
    all_nuts |>
      ## For testing:
      # bind_rows(tibble(Code="DK",Country="Denmark",Level=2,NUTS="DKY!",Label="")) |>
      filter(Level==0) |>
      left_join(
        nuts3 |>
          mutate(NUTS = str_sub(NUTS, 1L, 2L)) |>
          group_by(NUTS) |>
          summarise(geometry = st_union(geometry) |> st_make_valid(),
                    .groups="drop"),
        by="NUTS") |>
      mutate(geometry = st_make_valid(geometry), centroid = st_centroid(geometry, of_largest_polygon=TRUE)) |>
      select(NUTS, Code, Country, Level, Label, centroid, geometry) |>
      st_as_sf(sf_column_name = "geometry", crs=st_crs(map)) ->
      nuts0

    if(!all(st_is_valid(nuts0$geometry))) stop("Invalid geometries created by union to NUTS0")
    if(any(st_is_empty(nuts0$geometry))) stop("Missing geometries after union to NUTS0")
    # ggplot(nuts0) + geom_sf() + geom_sf_text(aes(geometry=centroid, label=NUTS))

    map <- bind_rows(
      nuts0,
      nuts1,
      nuts2,
      nuts3
    )

    attr(map, "version") <- hexscape_version()
    attr(map, "eurostat_path") <- eurostat_path

    # ggplot(map) + geom_sf() + geom_sf_text(aes(geometry=centroid, label=NUTS))
    qsave(map, savename)
  }

  if(nrow(map)==0L) stop("An unknown error occured fetching a map for the specified area")

  if(union){
    stop("TODO")
  }

  map |>
    filter(NUTS %in% nuts_code) |>
    select(Code, Country, NUTS, Level, Label, centroid, geometry) |>
    st_make_valid() ->
    map

  class(map) <- c("hs_map", class(map))

  return(map)

}
