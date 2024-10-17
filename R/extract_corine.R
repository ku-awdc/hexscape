#' @name extract_corine
#' @title Extract land use data from raw polygon file corresponding to a specific map area
#'
#' @param map
#' @param use_cache
#' @param simplify_keep
#' @param verbose
#'
#' @importFrom pbapply pblapply pbsapply
#' @importFrom sf st_read st_union st_layers st_transform st_crs st_intersects st_intersection
#' @importFrom checkmate assert
#' @importFrom dplyr rename_with
#'
#' @export
extract_corine <- function(map, corine_path, verbose=1L){

  ## TODO: rename extract/read_map/corine for consistency???

  assert(inherits(map, "sf"))
  mapsf <- st_union(map)

  if(verbose>0L) cat("Extracting information on layers and codes...\n")
  layers <- st_layers(corine_path)
  st_layers(corine_path)$name |>
    as.list() |>
    set_names() |>
    map( ~
      corine_path |>
        st_read(query = str_c("SELECT DISTINCT Code_18 FROM ", .x), layer=.x, quiet=TRUE) |>
        suppressWarnings() |>
        rename_with(toupper)
    ) |>
    bind_rows(.id="Layer") |>
    distinct(CODE_18) |>
    arrange(CODE_18) |>
    pull(CODE_18) ->
    codes

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

    ## Read the data from the corine file:
    layers$name |>
      set_names() |>
      lapply(function(l) suppressWarnings(st_read(corine_path, query = str_c("SELECT * FROM ", l, " WHERE Code_18 = ", code), layer=l, quiet=TRUE))) |>
      {\(x){ x[sapply(x,nrow)>0L] }}() |>
      lapply(function(x){
        x |>
          set_names(case_when(colnames(x) %in% c("Shape", "Layer") ~ colnames(x), TRUE ~ toupper(colnames(x)))) |>
          st_transform(st_crs(mapsf))
      }) |>
      bind_rows() ->
      obj

    obj |>
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
      ## TODO: add missing explicitly to data frame produced
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

  return(corine_raw)


  ## OLDER CODE BELOW HERE


  ## Try block for processing:
  ss <- try({


    ## Then save using qs:
    # qs::qsave(corine_raw, "DK0N16_full.rqs", preset="archive")
    # qs::qsave(corine_aggr, "LU0N16.rqs", preset="archive")

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
    fn <- file.path(getwd(), paste0("corine_failed_", strftime(Sys.time(), format="%Y%m%d%H%M%S"), ".rda"))
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


  ## OLDER CODE


  stopifnot(is.data.frame(map), )
  mapsf <- st_union(map) |> st_make_valid()
  # ggplot(mapsf) + geom_sf()
  if(length(mapsf)!=1L || !st_is_valid(mapsf) || any(!st_is_valid(map))) stop("Invalid map supplied")

  ## See if we need to re-load the local cache of qs files by CLC:
  stopifnot(isTRUE(use_cache) || isFALSE(use_cache))
  if(use_cache){

    clc_cache <- file.path(hexscape_getOption("storage_folder"), "processed_data", "clc_by_code")
    cache_ok <- validate_corine_cache()

    if(!cache_ok){
      if(verbose > 0L) cat("Extracting CLC cache (reason: ", attr(cache_ok, "reason"), ")...\n", sep="")
      info <- regenerate_corine_cache(verbose=verbose)
    }else{
      info <- qread(file.path(clc_cache, "info.rqs"))
    }
    codes <- info

  }else{

    if(verbose > 1L) cat("Extracting information on layers and codes...\n", sep="")
    codes <- as.list(st_layers(corine_path)$name) %>%
      set_names() %>%
      map_df( ~
                suppressWarnings(st_read(corine_path, query = str_c("SELECT DISTINCT Code_18 FROM ", .x), layer=.x, quiet=TRUE)) %>%
                `colnames<-`(., toupper(colnames(.)))
              , .id="Layer") %>%
      distinct(CODE_18) |>
      arrange(CODE_18) |>
      pull(CODE_18)
  }

  ## Then do the extraction:
  layers <- st_layers(corine_path)
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

    if(use_cache){
      ## Re-load the code and filter to only those that intersect anything we are interested in:
      obj <- qread(file.path(clc_cache, str_c("clc_", code, ".rqs")))
    }else{
      ## Read the data from the corine file:
      obj <- layers$name %>%
        `names<-`(.,.) %>%
        set_names() %>%
        lapply(function(l) suppressWarnings(st_read(corine_path, query = str_c("SELECT * FROM ", l, " WHERE Code_18 = ", code), layer=l, quiet=TRUE))) %>%
        `[`(sapply(.,nrow)>0) %>%
        lapply(function(x){
          x %>%
            `colnames<-`(., case_when(colnames(.) %in% c("Shape", "Layer") ~ colnames(.), TRUE ~ toupper(colnames(.)))) %>%
            st_transform(st_crs(mapsf))
        }) %>%
        bind_rows()
    }

    obj |>
      mutate(use = st_intersects(Shape, mapsf, sparse=FALSE)[,1]) |>
      filter(use) |>
      select(-use) ->
      obj

    if(nrow(obj)==0) return(retfun(NULL))
    return(retfun(obj))
  }

  if(verbose > 0L){
    if(use_cache){
      cat("Loading cached land use data and intersecting with map provided...\n", sep="")
    }else{
      cat("Extracting land use data and intersecting with map provided...\n", sep="")
    }
  }

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
        ## TODO: add missing explicitly to data frame produced
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

    ## Then save using qs:
    # qs::qsave(corine_raw, "DK0N16_full.rqs", preset="archive")
    # qs::qsave(corine_aggr, "LU0N16.rqs", preset="archive")

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
    fn <- file.path(getwd(), paste0("corine_failed_", strftime(Sys.time(), format="%Y%m%d%H%M%S"), ".rda"))
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

#' @rdname extract_corine
#' @export
validate_corine_cache <- function(){

  clc_cache <- file.path(hexscape_getOption("storage_folder"), "processed_data", "clc_by_code")
  if(!dir.exists(clc_cache)){
    dir.create(clc_cache)
  }
  if(!file.exists(file.path(clc_cache, "info.rqs"))){
    cache_ok <- FALSE
    attr(cache_ok, "reason") <- "no cache"
  }else{
    info <- qread(file.path(clc_cache, "info.rqs"))
    if(length(attr(info, "version"))==1L && attr(info, "version") >= package_version("0.4.5")){
      if(!all(str_c("clc_", info, ".rqs") %in% list.files(clc_cache))){
        cache_ok <- FALSE
        attr(cache_ok, "reason") <- "incomplete cache"
      }else{
        cache_ok <- TRUE
        attr(cache_ok, "reason") <- "OK"
      }
    }else{
      cache_ok <- FALSE
      attr(cache_ok, "reason") <- "out-dated cache"
    }
  }
  return(cache_ok)

}

#' @rdname extract_corine
#' @export
regenerate_corine_cache <- function(verbose=1L){

  corine_path <- file.path(hexscape_getOption("storage_folder"), "raw_data", "u2018_clc2018_v2020_20u1_geoPackage/DATA/U2018_CLC2018_V2020_20u1.gpkg")
  clc_cache <- file.path(hexscape_getOption("storage_folder"), "processed_data", "clc_by_code")

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
      lapply(function(x){
        x %>%
          `colnames<-`(., case_when(colnames(.) %in% c("Shape", "Layer") ~ colnames(.), TRUE ~ toupper(colnames(.)))) %>%
          st_transform(st_crs(mapsf))
      }) %>%
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

  invisible(info)
}
