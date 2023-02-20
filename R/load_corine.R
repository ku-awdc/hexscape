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
#' @importFrom sf st_read st_union st_layers st_transform st_crs st_intersects st_intersection st_make_valid st_is_valid st_dimension st_as_sf st_centroid
#' @importFrom rmapshaper ms_simplify
#' @importFrom units set_units
#' @importFrom qs qsave qread
#' @importFrom rlang set_names
#' @importFrom runjags new_unique
#'

#' @rdname load_corine
#' @export
cache_all_corine <- function(exclude = character(0), randomise=FALSE, verbose=1L, corine_path=file.path(hexscape_getOption("storage_folder"), "raw_data", "u2018_clc2018_v2020_20u1_geoPackage/DATA/U2018_CLC2018_V2020_20u1.gpkg")){


  ## Get all maps:
  cat("Loading all maps...\n")
  nuts_codes |>
    filter(Level==0) |>
    pull(NUTS) |>
    as.list() |>
    pblapply(load_map, verbose=0L) ->
  maps_unused

  ## Get all NUTS1 codes:
  nuts_codes |> filter(Level==1, !NUTS %in% nuts1_no_corine) |> pull(NUTS) -> nuts1

  ## Randomise if necessary:
  if(randomise){
    nuts1 <- nuts1[sample.int(length(nuts1))]
  }

  ## Excludes:
  exclmtch <- vapply(toupper(exclude), function(x) str_detect(nuts1, x), logical(length(nuts1)))
  if(any(!apply(exclmtch,2,any))){
    cat("WARNING: One or more patterns supplied to exclude not found\n")
  }
  nuts1 <- nuts1[!apply(exclmtch,1,any)]

  ## Then run load_corine for everything:
  cat("Loading all corine data...\n")
  load_corine(nuts1, union=TRUE, verbose=verbose, corine_path=corine_path)

  invisible(NULL)
}

#' @rdname load_corine
#' @export
load_corine <- function(nuts_code, union=FALSE, verbose=1L, corine_path=file.path(hexscape_getOption("storage_folder"), "raw_data", "u2018_clc2018_v2020_20u1_geoPackage/DATA/U2018_CLC2018_V2020_20u1.gpkg")){

  ## Start by checking the nuts_code(s) are valid:
  invalid <- which(!nuts_code %in% nuts_codes[["NUTS"]])
  if(length(invalid)>0L){
    stop("The following NUTS codes supplied are invalid: ", str_c(nuts_code[invalid], collapse=", "))
  }

  tibble(NUTS = nuts_code) |>
    left_join(
      nuts_codes |> filter(NUTS %in% nuts_code),
      by="NUTS"
    ) ->
    nuts_using

  ## Work out which nuts1 this refers to:
  bind_rows(
    nuts_using |>
      filter(Level==0) |>
      select(Code, NUTS) |>
      left_join(
        nuts_codes |> filter(Level==1) |> select(Code, NUTS1=NUTS),
        by="Code",
        multiple = "all"
      )
    ,
    nuts_using |>
      filter(Level >= 1L) |>
      mutate(NUTS1 = str_sub(NUTS, 1L, 3L))
    ) |>
    select(Code, NUTS, NUTS1) ->
      nuts1_codes

  ## Then do the extraction per NUTS1 code:
  storage_folder <- hexscape_getOption("storage_folder")

  nuts1_codes |>
    distinct(Code, NUTS, NUTS1) |>
    mutate(Savename = file.path(storage_folder, "processed_data", str_c("corine_", NUTS1, ".rqs")), Exists=file.exists(Savename)) |>
    group_by(NUTS1, Exists) |>
    mutate(Row = if_else(!Exists, cur_group_id(), NA_integer_)) |>
    # mutate(Row = as.numeric(factor(if_else(!Exists, cur_group_id(), NA_integer_)))) |>
    ungroup() |>
    mutate(Row = as.numeric(factor(Row)), NRow = if(all(is.na(Row))) 0L else max(Row, na.rm=TRUE)) |>
    group_split(.order=factor(NUTS1, levels=NUTS1)) |>
    lapply(function(cc){

      nuts_code <- cc[["NUTS1"]] |> unique()
      country_code <- cc[["Code"]] |> unique()
      stopifnot(length(nuts_code)==1L, length(country_code)==1L)

      savename <- cc[["Savename"]]
      if(file.exists(savename)){
        nc <- qread(savename)
        if(length(attr(nc, "version"))==1L && attr(nc, "version") >= package_version("0.4.3")){
          if(verbose > 1L) cat("Returning cached corine data for ", nuts_code, "\n", sep="")
          cache_ok <- TRUE
        }else{
          cache_ok <- FALSE
          if(verbose > 0L) cat("Re-processing corine data for ", nuts_code, " (required for new hexscape version)...\n", sep="")
        }
      }else{
        cache_ok <- FALSE
        if(verbose > 0L) cat("Processing corine data for ", nuts_code, " (", cc[["Row"]][1L], " of ", cc[["NRow"]][1L], ")...\n", sep="")
      }

      if(!cache_ok){
        ss <- try({

        ## Then extract maps at NUTS3 level corresponding to this NUTS1:
        nuts_codes |>
          filter(Level==3, str_sub(NUTS,1L,3L)==nuts_code) |>
          pull(NUTS) ->
        nuts3

        nm <- load_map(nuts3) |> select(Code, Country, NUTS, geometry)
        stopifnot(nrow(nm)==length(nuts3))

        ## Then pass to extract_corine:
        nc <- extract_corine(nm, verbose=verbose, corine_path=corine_path)

        # Then save the result:
        qsave(nc, savename)
        })

        # If there was a problem then log it and continue:
        if(inherits(ss, "try-error")){
          warning(str_c("Extracting corine data for ", nuts_code, " failed"))
          return(NULL)
        }
      }

      ## Then filter out what is needed for each of the NUTS originally requested:
      nuts_codes |>
        filter(NUTS %in% cc[["NUTS"]]) |>
        group_by(NUTS) |>
        group_split() |>
        lapply(function(x){
          stopifnot(nrow(x)==1L)
          if(x[["Level"]] %in% c(0L, 1L)){
            nc |>
              mutate(NUTS_REQ = x[["NUTS"]]) ->
              rv
          }else{
            nc |>
              filter(str_detect(NUTS, str_c("^", x[["NUTS"]]))) |>
              mutate(NUTS_REQ = x[["NUTS"]]) ->
              rv
          }
          rv
        }) |>
        bind_rows() |>
        mutate(Code = country_code, NUTS1 = nuts_code) ->
        corine_filtered

      if(union){
        corine_filtered |>
          group_by(Code, NUTS1, CLC) |>
          summarise(Area = sum(Area), Area_simplified = sum(Area_simplified),
                    geometry = geometry |> st_union() |> st_make_valid(),
                    .groups="drop") ->
          corine_filtered

        if(any(!st_is_valid(corine_filtered))){
          warning("One or more invalid geometry caused by st_union for ", str_c(cc[["NUTS"]], collapse=", "))
        }
      }

      ## And return:
      return(corine_filtered)
    }) ->
  all_corine

  if(any(sapply(all_corine, is.null))){
    stop("One or more nuts_code could not be extracted from the data")
  }
  all_corine <- bind_rows(all_corine)

  ## Optionally do union and make valid:
  if(union){
    all_corine |>
      group_by(CLC) |>
      summarise(Area = sum(Area), Area_simplified = sum(Area_simplified),
                geometry = geometry |> st_union() |> st_make_valid(),
                .groups="drop") ->
      all_corine

    if(any(!st_is_valid(all_corine))){
      warning("One or more invalid geometry caused by final st_union")
    }
  }else{
    all_corine <- all_corine |>
      select(-NUTS_REQ, -NUTS1)
  }

  ## Then add CLC labels:
  clc_codes |> select(-CLC_RGB) |>
    right_join(all_corine, by="CLC") |>
    st_as_sf(sf_column_name = "geometry", crs=st_crs(all_corine)) ->
  final_corine

  if(verbose > 0L){
    if(union){
      cat("Returning corine data (union by CLC)\n", sep="")
    }else{
      cat("Returning corine data (by CLC and NUTS3 area)\n", sep="")
    }
  }

  return(final_corine)
}


#' @rdname load_corine
#' @export
extract_corine <- function(map, simplify_keep=0.25, corine_path=file.path(hexscape_getOption("storage_folder"), "raw_data", "u2018_clc2018_v2020_20u1_geoPackage/DATA/U2018_CLC2018_V2020_20u1.gpkg"), verbose=1L){

  stopifnot(is.data.frame(map), inherits(map, "sf"))
  mapsf <- st_union(map) |> st_make_valid()
  # ggplot(mapsf) + geom_sf()
  if(length(mapsf)!=1L || !st_is_valid(mapsf) || any(!st_is_valid(map))) stop("Invalid map supplied")

  ## See if we need to re-load the local cache of qs files by CLC:
  clc_cache <- file.path(hexscape_getOption("storage_folder"), "processed_data", "clc_by_code")
  if(!dir.exists(clc_cache)){
    stop("CLC cache folder (", clc_cache, ") does not exist")
  }
  if(!file.exists(file.path(clc_cache, "info.rqs"))){
    if(verbose > 0L) cat("Extracting raw CLC code information...\n", sep="")
    cache_ok <- FALSE
  }else{
    info <- qread(file.path(clc_cache, "info.rqs"))
    if(length(attr(info, "version"))==1L && attr(info, "version") >= package_version("0.4.3")){
      cache_ok <- TRUE
    }else{
      cache_ok <- FALSE
      if(verbose > 0L) cat("Re-extracting raw CLC code information (required for new hexscape version)...\n", sep="")
    }
  }

  if(!cache_ok){

    if(verbose > 1L) cat("Extracting information on layers and codes...\n", sep="")
    layers <- st_layers(corine_path)
    codes <- as.list(layers$name) %>%
      set_names() %>%
      map_df( ~
                suppressWarnings(st_read(corine_path, query = str_c("SELECT DISTINCT Code_18 FROM ", .x), layer=.x, quiet=TRUE)) %>%
                `colnames<-`(., toupper(colnames(.)))
              , .id="Layer") %>%
      distinct(CODE_18) |>
      arrange(CODE_18) |>
      pull(CODE_18)

    st <- Sys.time()
    get_clc <- function(cc){

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

      ## Extract the code and cache:
      obj <- layers$name %>%
        `names<-`(.,.) %>%
        set_names() %>%
        lapply(function(l) suppressWarnings(st_read(corine_path, query = str_c("SELECT * FROM ", l, " WHERE Code_18 = ", code), layer=l, quiet=TRUE))) %>%
        `[`(sapply(.,nrow)>0) %>%
        lapply(function(x) x %>% `colnames<-`(., case_when(colnames(.) %in% c("Shape", "Layer") ~ colnames(.), TRUE ~ toupper(colnames(.))))) %>%
        bind_rows()

      ## Then save:
      qsave(obj, file=file.path(clc_cache, str_c("clc_", code, ".rqs")))

      return(code)
    }

    if(verbose > 1L) cat("Extracting raw land use data relating to ", length(codes), " CLC codes...\n", sep="")

    # Note: verbose=0 - no update, verbose>2 - detailed update with lapply
    if(verbose %in% c(1L,2L)) afun <- pblapply else afun <- lapply
    length(codes) |>
      # sample.int to randomise ordering (more realistic time to completion)
      sample.int() |>
      afun(get_clc) |>
      simplify2array() ->
      info

    ## Then save the codes and file:
    attr(info, "version") <- hexscape_version()
    attr(info, "corine_path") <- corine_path

    qsave(info, file=file.path(clc_cache, "info.rqs"))
  }

  ## Then do the extraction per NUTS3 area:
  codes <- info

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

    ## Re-load the code and filter to only those that intersect anything we are interested in:
    qread(file.path(clc_cache, str_c("clc_", code, ".rqs"))) |>
      st_transform(st_crs(map)) |>
      mutate(use = st_intersects(Shape, mapsf, sparse=FALSE)[,1]) |>
      filter(use) |>
      select(-use) ->
      obj

    if(nrow(obj)==0) return(retfun(NULL))
    return(retfun(obj))
  }

  if(verbose > 0L) cat("Extracting land use data and intersecting with map provided...\n", sep="")

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
      if(propmiss > 0.001){
        if(verbose > 0L) cat("WARNING:", msg, "\n")
        warning(msg)
      }

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

    ## Aggregate within supplied rows (if data frame) and aggregate:
    if(verbose > 0L){
      if(simplify_keep < 1.0){
        cat("Extracting and simplifying polygons for ", nrow(map), " spatial areas...\n", sep="")
      }else{
        cat("Extracting polygons for ", nrow(map), " spatial areas...\n", sep="")
      }
    }
    st <- Sys.time()
    seq_len(nrow(map)) |>
      as.list() |>
      afun(function(r){

        if(verbose > 2L){
          if("NUTS" %in% names(map)){
            cat("Extracting polygons for ", map[["NUTS"]][r], " ... ", sep="")
          }else{
            cat("Extracting polygons for map row ", r, " ... ", sep="")
          }

          ## hack:
          retfun <- function(rv){
            cat(round(cc/length(codes)*100), "% complete after ", round(as.numeric(Sys.time()-st, units="mins")), " minutes\n", sep="")
            return(rv)
          }
        }else{
          retfun <- function(rv) rv
        }

        nm <- map |> slice(r)

        corine_raw |>
          filter(st_intersects(Shape, nm, sparse=FALSE)[,1]) |>
          mutate(Shape = st_intersection(Shape, nm) |> st_make_valid()) |>
          select(CLC = CODE_18, Shape) |>
          group_by(CLC) |>
          summarise(geometry = st_union(Shape) |> st_make_valid(),
                    .groups="drop") |>
          mutate(Area = st_area(geometry) |> set_units(km^2) |> as.numeric()) |>
          arrange(CLC) ->
          corine_aggr

        if(any(!st_is_valid(corine_aggr))){
          stop("One or more invalid polygon in original (aggregated) corine data")
        }

        if(simplify_keep < 1.0){
          if(verbose > 2L) cat("Simplifying polygons...\n", sep="")

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

        rv <- bind_cols(
          nm |> st_drop_geometry(),
          corine_simplified
        ) |>
          st_as_sf(sf_column_name = "geometry", crs=st_crs(corine_simplified))

        return(retfun(rv))
      }) |>
      bind_rows() ->
    corine_simplified

    if(!all(st_is_valid(corine_simplified))) stop("Invalid polygons created")
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
  attr(corine_simplified, "simplify_keep") <- simplify_keep
  attr(corine_simplified, "version") <- hexscape_version()

  if(verbose > 1L) cat("Done\n", sep="")
  return(corine_simplified)

}

## Manual list of NUTS1 codes that don't have Corine coverage:
nuts1_no_corine <- c("FRY")
