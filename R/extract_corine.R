#' @name read_corine
#' @title Read land use data from raw polygon file corresponding to a specific map area
#'
#' @param map
#' @param corine_path
#' @param type one of "reduced" (only CLC and Shape columns with original features), "grouped" (as for "grouped" but same CLC codes joined into multi-polygons), or "full" (all columns and rows in underlying data).
#' @param intersection
#' @param max_rows target maxinum number of features to retrieve at a time - this represents a trade off between speed and memory footprint. Special values of 0 means extract each CLC individually (minimum memory requirement), and Inf means all features simultaneously (around 30GB available memory required, for type "grouped" or "reduced").
#' @param verbose
#'
#' @importFrom pbapply pblapply pbsapply
#' @importFrom sf st_read st_union st_layers st_transform st_crs st_intersects st_intersection st_make_valid
#' @importFrom checkmate assert qassert
#' @importFrom dplyr rename_with
#'

#' @rdname read_corine
#' @export
read_corine <- function(nuts_code, year="2021", corine_path, max_rows = 0L, verbose=1L){

  qassert(nuts_code, "S1")
  stopifnot(nuts_code %in% all_nuts_codes(level=1L)[["NUTS"]])

  if(is.numeric(year)) year <- as.character(year)
  map_year <- match.arg(year)

  qassert(corine_path, "S1")
  corine_year <- "2018"
  ## TODO: validate corine_path and work out corine_year

  map <- load_map(nuts_code, year=map_year)
  corine_raw <- extract_corine(map, corine_path, type="reduced", intersection=TRUE, max_rows=max_rows, verbose=verbose)
  attr(corine_raw, "nuts1") <- nuts_code
  attr(corine_raw, "map_year") <- map_year
  attr(corine_raw, "corine_year") <- corine_year
  attr(corine_raw, "hexscape_version") <- packageVersion("hexscape")

  ## Save:
  ddir <- hexscape:::hs_data_dir("corine", corine_year, create_subdir=TRUE)
  qs::qsave(corine_raw, file.path(ddir, str_c(nuts_code, ".rqs")), preset="archive")

  invisible(file.path(ddir, str_c(nuts_code, ".rqs")))
}

#' @rdname read_corine
#' @export
extract_corine <- function(map, corine_path, type=c("reduced","grouped","full"), intersection=TRUE, max_rows = 0L, verbose=1L){

  ## TODO: rename extract/read_map/corine for consistency???

  assert(inherits(map, "sf"))
  mapsf <- st_union(map) |> st_make_valid()

  qassert(corine_path, "S1")
  qassert(max_rows, "N1[0,]")

  type <- match.arg(type)
  qassert(intersection, "B1")

  ## TODO: cache this in the package environment (against file name):
  if(verbose>0L) cat("Extracting information on layers and codes...\n")
  layers <- st_layers(corine_path)
  st_layers(corine_path)$name |>
    as.list() |>
    set_names() |>
    map( ~
        corine_path |>
        # st_read(query = str_c("SELECT DISTINCT Code_18 FROM ", .x), quiet=TRUE) |>
        # suppressWarnings() |>
        st_read(query = str_c("SELECT Code_18, COUNT(*) FROM ", .x, " GROUP BY Code_18"), quiet=TRUE) |>
        rename_with(toupper)
    ) |>
    bind_rows(.id="Layer") |>
    group_by(CLC = CODE_18) |>
    summarise(Total = sum(`COUNT...`), .groups="drop") |>
    arrange(desc(Total)) ->
    codes

  ## Determine number of chunks to use:
  chunks <- if(is.infinite(max_rows)) 1L else min(nrow(codes), ceiling(sum(codes[["Total"]])/max_rows))
  code_list <- lapply(seq_len(chunks), function(x) numeric(0))
  tally <- numeric(chunks)
  for(i in seq_along(codes[["Total"]])){
    c <- which.min(tally)[1L]
    tally[c] <- tally[c] + codes[["Total"]][i]
    code_list[[c]] <- c(code_list[[c]], codes[["CLC"]][i])
  }
  code_list <- lapply(code_list, sort)
  stopifnot(
    codes[["CLC"]] %in% unlist(code_list),
    unlist(code_list) %in% codes[["CLC"]],
    length(unlist(code_list))==nrow(codes)
  )

  st <- Sys.time()
  get_corine_sf <- function(cc){

    code <- code_list[[cc]]
    if(verbose > 2L){
      cat("Extracting codes: ", str_c(code,collapse=","), " ... ", sep="")

      ## hack:
      retfun <- function(rv){
        cat(round(cc/length(codes)*100), "% complete after ", round(as.numeric(Sys.time()-st, units="mins")), " minutes\n", sep="")
        return(rv)
      }
    }else{
      retfun <- identity
    }

    if(type%in%c("grouped","reduced")){
      # Note: this is marginally faster (and probably uses less memory) than *:
      selst <- "SELECT Code_18 AS CLC, Shape "
    }else if(type=="full"){
      selst <- "SELECT * "
    }else{
      stop("Unhandled value for type")
    }

    if(chunks==1L){
      qryfun <- function(l) str_c(selst, "FROM ", l)
    }else if(chunks==nrow(codes)){
      # Note: this is no faster than IN...
      qryfun <- function(l) str_c(selst, "FROM ", l, " WHERE Code_18 = ", code)
    }else{
      qryfun <- function(l) str_c(selst, "FROM ", l, " WHERE Code_18 IN (", str_c(code, collapse=","), ")")
    }

    ## Read the data from the corine file and extract only
    ## features that intersect with the supplied map:
    layers$name |>
      set_names() |>
      lapply(function(l){
        st_read(corine_path, query = qryfun(l), quiet=TRUE) |>
          rename_with(\(x) case_when(x=="Shape" ~ x, str_detect(toupper(x), "^CODE_") ~ "CLC", TRUE ~ toupper(x))) ->
          df
        if(nrow(df)==0L) return(NULL)
        df |>
          st_transform(st_crs(mapsf)) |>
          filter(st_intersects(Shape, mapsf, sparse=FALSE)[,1]) |>
          mutate(Shape = st_make_valid(Shape))
      }) |>
      bind_rows() ->
      obj

    ## Optionally reduce to intersection of the map:
    if(intersection){
      obj <- obj |> mutate(Shape = st_intersection(Shape, mapsf))
    }

    ## Optionally convert to multipolygon:
    if(type%in%c("grouped") && nrow(obj) > 0L){
      obj |>
        group_by(CLC) |>
        summarise(Shape = st_union(Shape), .groups="drop") |>
        arrange(CLC) ->
        obj
    }

    if(nrow(obj)==0) return(retfun(NULL))
    return(retfun(obj))
  }

  if(verbose > 0L) cat("Extracting land use data for ", nrow(codes), " codes", if(!chunks %in% c(1,nrow(codes))) str_c(" in ", chunks, " batches"), "...\n", sep="")
  # Note: verbose=0 - no update, verbose>2 - detailed update with lapply
  if(chunks > 1L && verbose %in% c(1L,2L)) afun <- pblapply else afun <- lapply
  chunks |>
    # sample.int to randomise ordering (more realistic time to completion)
    sample.int() |>
    afun(get_corine_sf) |>
    bind_rows() ->
    cr

  ## Add in areas not covered by corine as missing land use type:
  # Special case of no matching corine data:
  if(verbose > 0L) cat("Determining overall corine coverage...\n", sep="")
  if(nrow(cr)==0L){
    missingcc <- mapsf
  }else{
    missingcc <- st_difference(mapsf, st_union(cr[["Shape"]])) |> st_make_valid()
  }
  if(!all(is.na(st_dimension(missingcc)))){
    propmiss <- as.numeric( st_area(missingcc) / st_area(mapsf) )
    msg <- str_c("Corine coverage for supplied area is incomplete (", round(1-propmiss,3)*100, "%)")
    if(propmiss > 0.001){
      if(verbose > 0L) cat(msg, "\n")
      warning(msg)
    }

    tibble(OBJECTID=0L, CLC = NA_character_, REMARK = NA_character_, AREA_HA = 0.0, ID = "MISSING_CC", Shape = missingcc) |>
      st_as_sf() ->
      corine_blank
    if(type %in% c("grouped","reduced")){
      corine_blank <- corine_blank |> select(CLC, Shape)
    }

    if(nrow(cr)==0L){
      corine_raw <- corine_blank
    }else{
      corine_raw <- bind_rows(cr, corine_blank)
    }
  }else{
    corine_raw <- cr
  }

  if(verbose>0L) cat("Done in ", round(as.numeric(Sys.time()-st, units="mins"), 1), " minutes.\n")

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
