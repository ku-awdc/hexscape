#' @name load_corine
#' @title Extract land use data for a specific country
#'
#' @param nuts_code
#' @param clc_key
#' @param corine_path
#' @param verbose
#' @param map
#' @param simplify_keep passed on to \code{\link[rmapshaper]{ms_simplify}}
#'
#' @importFrom pbapply pblapply pbsapply
#' @importFrom sf st_read st_union st_layers st_transform st_crs st_intersects st_intersection st_make_valid st_is_valid st_dimension
#' @importFrom rmapshaper ms_simplify
#' @importFrom units set_units
#' @importFrom qs qsave qread
#' @importFrom runjags new_unique
#'

#' @rdname load_corine
#' @export
cache_all_corine <- function(corine_path=file.path(hexscape_getOption("storage_folder"), "raw_data", "u2018_clc2018_v2020_20u1_geoPackage/DATA/U2018_CLC2018_V2020_20u1.gpkg"), nuts_year=2016, verbose=1L){

  ## Get all NUTS1 codes:
  if(verbose > 0L) cat("Extracting all available NUTS1 codes...\n", sep="")
  qf <- quietly(get_eurostat_geospatial)
  qf(output_class = "sf", resolution = "01", nuts_level = '3',
     year = nuts_year, crs = "4326",
     cache_dir = file.path(hexscape_getOption("storage_folder"), "eurostat_cache"),
     make_valid = TRUE)[["result"]] |>
    st_make_valid() ->
    map

  map |>
    as_tibble() |>
    mutate(NUTS1 = str_sub(NUTS_ID, 1, 3)) |>
    distinct(CNTR_CODE, NUTS1) |>
    arrange(CNTR_CODE, NUTS1) |>
    ## Remove NUTS1 areas that are not in corine:
    filter(!NUTS1 %in% nuts1_no_corine) |>
    pull(NUTS1) ->
  nuts1

  ## Then run load_corine for everything:
  load_corine(nuts1, corine_path=corine_path, verbose=verbose)

  invisible(NULL)
}

#' @rdname load_corine
#' @export
load_corine <- function(nuts_code, clc_key=NULL, corine_path=file.path(hexscape_getOption("storage_folder"), "raw_data", "u2018_clc2018_v2020_20u1_geoPackage/DATA/U2018_CLC2018_V2020_20u1.gpkg"), verbose=1L, ...){

  ## Start by checking the nuts_code(s) are valid NUTS0 or NUTS1

  # Check there are either two characters or two characters plus character/number:
  valid <- str_detect(nuts_code, "^[[:ALPHA:]][[:ALPHA:]][[:ALPHA:][:digit:]]?") & str_length(nuts_code) %in% c(2,3)
  if(any(!valid)) stop("The following nuts codes are in an invalid format: ", str_c(nuts_code[!valid], collapse=", "))

  # Get map data for countries:
  countries <- tibble(NC=nuts_code, CC=nuts_code |> str_sub(1,2))

  countries |>
    distinct(CC) |>
    pull(CC) %>%
    `names<-`(.,.) %>%
    as.list() |>
    lapply(function(cc){
      load_map(cc, verbose=1L)
    }) |>
    bind_rows() ->
  maps

  # Get a full list of NUTS1 codes and check validity:
  if(verbose > 1L) cat("Checking validity of NUTS codes provided...\n", sep="")
  maps |>
    as_tibble() |>
    mutate(N1=str_sub(NUTS_ID,1,3)) |>
    distinct(CC=CNTR_CODE, N1) |>
    left_join(
      countries |> filter(NC==CC), by="CC"
    ) |>
    # Remove NUTS1 with no corine unless specifically requested:
    filter(is.na(NC) | !N1 %in% nuts1_no_corine) |>
    mutate(NC = if_else(is.na(NC), N1, NC)) |>
    right_join(countries |> select(NC), by="NC") ->
  countries

  if(any(is.na(countries$N1))){
    stop("The following nuts codes appear to be invalid: ", str_c(countries |> filter(is.na(N1)) |> pull(NC), collapse=", "))
  }

  ## Then do the extraction per NUTS1 code:
  storage_folder <- hexscape_getOption("storage_folder")

  countries |>
    distinct(CC, N1) |>
    mutate(Savename = file.path(storage_folder, "processed_data", str_c("corine_", N1, ".rqs")), Exists=file.exists(Savename)) |>
    arrange(CC, N1) |>
    group_by(Exists) |>
    mutate(Row = 1:n(), NRow = max(Row)) |>
    ungroup() |>
    group_by(CC, N1) |>
    group_split() |>
    lapply(function(cc){

      stopifnot(nrow(cc)==1L)
      nuts_code <- cc[["N1"]]
      country_code <- cc[["CC"]]

      savename <- cc[["Savename"]]
      if(file.exists(savename)){
        if(verbose > 1L) cat("Returning cached corine data for ", nuts_code, "\n", sep="")
        return(qread(savename))
      }

      if(verbose > 0L) cat("Processing corine data for ", nuts_code, " (", cc[["Row"]], " of ", cc[["NRow"]], ")...\n", sep="")

      ss <- try({
      ## Then filter the right nuts_codes from the map:
      nm <- maps |>
        filter(str_sub(NUTS_ID, 1L, 3L) == nuts_code) |>
        pull(geometry) |>
        st_union()

      # Then pass to extract_corine and add the NUTS and country codes:
      extract_corine(nm, corine_path=corine_path, verbose=verbose) |>
        mutate(Country = country_code, NUTS1 = nuts_code) |>
        select(Country, NUTS1, everything()) ->
      nc
      })

      # If there was a problem then log it and continue:
      if(inherits(ss, "try-error")){
        warning(str_c("Extracting corine data for ", nuts_code, " failed"))
        return(NULL)
      }

      # Then save the result:
      qsave(nc, savename)

      # And return:
      return(nc)
    }) ->
  all_corine

  if(any(sapply(all_corine, is.null))){
    stop("One or more nuts_code could not be extracted from the data")
  }

  all_corine <- bind_rows(all_corine)

  ## Either merge with a custom CLC key and do st_union or try to load CLC labels
  if(is.null(clc_key)){

  }
  browser()

  map <- extract_map(country_code=country_code, refresh=refresh, verbose=verbose, ...)

  stopifnot(nrow(map)>0)

  mapsf <- st_union(map$geometry)
  # ggplot(mapsf) + geom_sf()

  corine_path <- file.path(storage_folder, "raw_data", "u2018_clc2018_v2020_20u1_geoPackage/DATA/U2018_CLC2018_V2020_20u1.gpkg")

  if(verbose > 0L) cat("Extracting information on layers...\n", sep="")
  layers <- st_layers(corine_path)
  codes <- clc$CLC_CODE

  ## Make sure there aren't any non-recognised codes in any layer!!
  if(verbose > 0L) cat("Checking unique codes from each layer...\n", sep="")
  allcodes <- as.list(layers$name) %>%
    `names<-`(.,.) %>%
    map_df( ~
              suppressWarnings(st_read(corine_path, query = str_c("SELECT DISTINCT Code_18 FROM ", .x), layer=.x, quiet=TRUE)) %>%
              `colnames<-`(., toupper(colnames(.)))
            , .id="Layer")
  if(!all(allcodes$CODE_18 %in% codes)){
    stop(str_c("The following non-regognised CLC codes were found in one or more corine layer: ", str_c(allcodes$CODE_18[! allcodes$CODE_18 %in% codes], collapse=", ")))
  }

  st <- Sys.time()
  get_corine_sf <- function(cc){

    code <- codes[cc]
    if(verbose > 1L){
      cat("Extracting code ", code, " ... ", sep="")

      ## hack:
      retfun <- function(rv){
        cat(round(cc/length(codes)*100), "% complete after ", round(as.numeric(Sys.time()-st, units="mins")), " minutes\n", sep="")
        return(rv)
      }
    }else{
      retfun <- function(rv) rv
    }

    ## First extract the code and filter to only those that intersect anything we are interested in:
    obj <- layers$name %>%
      `names<-`(.,.) %>%
      as.list() %>%
      lapply(function(l) suppressWarnings(st_read(corine_path, query = str_c("SELECT * FROM ", l, " WHERE Code_18 = ", code), layer=l, quiet=TRUE))) %>%
      `[`(sapply(.,nrow)>0) %>%
      lapply(function(x) x %>% `colnames<-`(., case_when(colnames(.) %in% c("Shape", "Layer") ~ colnames(.), TRUE ~ toupper(colnames(.))))) %>%
      bind_rows()

    if(nrow(obj)==0) return(retfun(NULL))

    obj <- obj %>%
      st_transform(st_crs(map)) %>%
      mutate(use = st_intersects(Shape, mapsf, sparse=FALSE)[,1]) %>%
      filter(use) %>%
      select(-use)

    if(nrow(obj)==0) return(retfun(NULL))

    ## Then make intersections with each of the NUTS3 areas we have:
    o2 <- map %>%
      split(.$NUTS_ID) %>%
      map(
        ~ obj %>%
          mutate(use = st_intersects(Shape, .x$geometry, sparse=FALSE)[,1]) %>%
          filter(use) %>%
          select(-use) %>%
          mutate(Shape = st_intersection(Shape, .x$geometry), NUTS_ID=.x$NUTS_ID)
      ) %>%
      bind_rows()

    return(retfun(o2))
  }

  if(verbose > 0L) cat("Extracting land use data relating to ", length(codes), " CLC codes for ", country_code, "\n[This will take some time]\n", sep="")
  if(verbose==1L){
    corine_raw <- pblapply(seq_along(codes), get_corine_sf) %>%
      bind_rows() %>%
      select(CLC_CODE = CODE_18, everything())
  }else{
    corine_raw <- lapply(seq_along(codes), get_corine_sf) %>%
      bind_rows() %>%
      select(CLC_CODE = CODE_18, everything())
  }

  corine_sf <- corine_raw %>%
    left_join(clc, by="CLC_CODE") %>%
    left_join(map %>% as_tibble() %>% select(NUTS_ID, CNTR_CODE, NUTS_NAME), by="NUTS_ID") %>%
    select(CNTR_CODE, NUTS_ID, CLC_CODE, OBJECTID, AREA_HA, OBJECTID, Shape)

  if(verbose > 0L) cat("Saving results...\n", sep="")
  saveRDS(corine_sf, savename, compress=TRUE)

  if(verbose > 0L) cat("Done\n", sep="")

  corine_sf <- corine_sf %>%
    left_join(clc, by="CLC_CODE") %>%
    select(CNTR_CODE, NUTS_ID, CLC_CODE, CLC_LABEL1, CLC_LABEL2, CLC_LABEL3, everything())

  return(corine_sf)

}


#' @rdname load_corine
#' @export
extract_corine <- function(map, simplify_keep=0.2, corine_path=file.path(hexscape_getOption("storage_folder"), "raw_data", "u2018_clc2018_v2020_20u1_geoPackage/DATA/U2018_CLC2018_V2020_20u1.gpkg"), verbose=1L){

  if(is.data.frame(map)){
    mapsf <- st_union(map$geometry) |> st_make_valid()
  }else{
    mapsf <- st_union(map) |> st_make_valid()
  }
  # ggplot(mapsf) + geom_sf()
  if(length(mapsf)!=1L || !st_is_valid(mapsf)) stop("Invalid map supplied")

  if(verbose > 1L) cat("Extracting information on layers and codes...\n", sep="")
  layers <- st_layers(corine_path)
  codes <- as.list(layers$name) %>%
    `names<-`(.,.) %>%
    map_df( ~
              suppressWarnings(st_read(corine_path, query = str_c("SELECT DISTINCT Code_18 FROM ", .x), layer=.x, quiet=TRUE)) %>%
              `colnames<-`(., toupper(colnames(.)))
            , .id="Layer") %>%
    distinct(CODE_18) |>
    arrange(CODE_18) |>
    pull(CODE_18)

  ## Then do the extraction:
  st <- Sys.time()
  get_corine_sf <- function(cc){

    code <- codes[cc]
    if(verbose > 2L){
      cat("Extracting code ", code, " ... ", sep="")

      ## hack:
      retfun <- function(rv){
        cat(round(cc/length(codes)*100), "% complete after ", round(as.numeric(Sys.time()-st, units="mins")), " minutes\n", sep="")
        return(rv)
      }
    }else{
      retfun <- function(rv) rv
    }

    ## First extract the code and filter to only those that intersect anything we are interested in:
    obj <- layers$name %>%
      `names<-`(.,.) %>%
      as.list() %>%
      lapply(function(l) suppressWarnings(st_read(corine_path, query = str_c("SELECT * FROM ", l, " WHERE Code_18 = ", code), layer=l, quiet=TRUE))) %>%
      `[`(sapply(.,nrow)>0) %>%
      lapply(function(x) x %>% `colnames<-`(., case_when(colnames(.) %in% c("Shape", "Layer") ~ colnames(.), TRUE ~ toupper(colnames(.))))) %>%
      bind_rows()

    if(nrow(obj)==0) return(retfun(NULL))

    obj <- obj %>%
      st_transform(st_crs(map)) %>%
      mutate(use = st_intersects(Shape, mapsf, sparse=FALSE)[,1]) %>%
      filter(use) %>%
      select(-use)

    if(nrow(obj)==0) return(retfun(NULL))
    return(retfun(obj))
  }

  if(verbose > 1L) cat("Extracting land use data relating to ", length(codes), " CLC codes (this may take some time)...\n", sep="")

  # Note: verbose=0 - no update, verbose>2 - detailed update with lapply
  if(verbose %in% c(1L,2L)) afun <- pblapply else afun <- lapply
  length(codes) |>
    # sample.int to randomise ordering (more realistic time to completion)
    sample.int() |>
    afun(get_corine_sf) |>
    bind_rows() ->
  cr

  ## Try block for processing:
  ss <- try({

    ## Add in areas not covered by corine as missing land use type:
    # Special case of no matching corine data:
    if(verbose > 1L) cat("Determining overall corine coverage...\n", sep="")
    if(nrow(cr)==0L){
      missingcc <- mapsf
    }else{
      missingcc <- st_difference(mapsf |> st_make_valid(), st_union(cr$Shape) |> st_make_valid()) |> st_make_valid()
    }
    if(!all(is.na(st_dimension(missingcc)))){
      propmiss <- as.numeric( st_area(missingcc) / st_area(mapsf) )
      msg <- str_c("Corine coverage for supplied area is incomplete (", round(1-propmiss,3)*100, "%)")
      if(verbose > 0L) cat("WARNING:", msg, "\n")
      warning(msg)

      if(nrow(cr)==0L){
        corine_raw <- tibble(OBJECTID=0L, CODE_18 = NA_character_, REMARK = NA_character_, AREA_HA = 0.0, ID = "MISSING_CC", Shape = missingcc) |> st_as_sf()
      }else{
        corine_raw <- cr |>
          bind_rows(
            tibble(OBJECTID=0L, CODE_18 = NA_character_, REMARK = NA_character_, AREA_HA = 0.0, ID = "MISSING_CC", Shape = missingcc) |> st_as_sf()
          )
      }
    }else{
      corine_raw <- cr
    }

    ## Calculate areas and aggregate:
    corine_raw |>
      mutate(Shape = st_intersection(Shape, mapsf) |> st_make_valid()) |>
      mutate(Area = st_area(Shape) |> set_units(km^2) |> as.numeric()) |>
      select(CLC = CODE_18, Area, Shape) |>
      group_by(CLC) |>
      summarise(Area = sum(Area),
                geometry = st_union(Shape) |> st_make_valid(),
                .groups="drop") |>
      arrange(CLC) ->
      corine_aggr

    if(any(!st_is_valid(corine_aggr))){
      stop("One or more invalid polygon in original (aggregated) corine data")
    }

    ## Simplify:
    if(simplify_keep < 1.0){
      if(verbose > 1L) cat("Simplifying polygons...\n", sep="")

      corine_aggr |>
        mutate(geometry = ms_simplify(geometry, keep=simplify_keep, method="dp", keep_shapes=TRUE) |> st_make_valid()) |>
        mutate(Area_simplified = st_area(geometry) |> set_units(km^2) |> as.numeric()) |>
        select(CLC, Area, Area_simplified, geometry) ->
        corine_simplified

      if(any(!st_is_valid(corine_simplified))){
        stop("One or more invalid polygon in simplified corine data")
      }
    }else{
      corine_aggr |>
        mutate(Area_simplified = st_area(geometry) |> set_units(km^2) |> as.numeric()) |>
        select(CLC, Area, Area_simplified, geometry) ->
        corine_simplified
    }

  })
  if(inherits(ss, "try-error")){
    if(verbose > 0L) cat("ERROR:", as.character(ss), "\n")
    fn <- file.path(getwd(), runjags::new_unique("corine_failed", "rda"))
    save(mapsf, cr, simplify_keep, file=fn)
    stop(str_c("There was an unexpected problem processing corine data - intermediate output has been saved to ", fn))
  }

  # ggplot(corine_raw, aes(geometry=Shape, fill=CLC)) + geom_sf(lwd=0, color=NA) + theme_void() + theme(legend.pos="none")
  # ggplot(corine_simplified, aes(geometry=geometry, fill=CLC)) + geom_sf(lwd=0, color=NA) + theme_void() + theme(legend.pos="none")

  ## Add attributes for filename and map:
  attr(corine_simplified, "corine_path") <- corine_path
  attr(corine_simplified, "map") <- mapsf
  attr(corine_simplified, "simplify_keep") <- simplify_keep
  attr(corine_simplified, "version") <- hexscape_version()

  if(verbose > 1L) cat("Done\n", sep="")
  return(corine_simplified)

}

## Manual list of NUTS1 codes that don't have Corine coverage:
nuts1_no_corine <- c("FRY")
