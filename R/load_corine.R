#' @name load_corine
#' @title Extract land use data for a specific country
#'
#' @param nuts_code
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
cache_all_corine <- function(exclude = character(0), randomise=FALSE, verbose=1L){


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
  nuts1 |>
    as.list() |>
    lapply(function(n1){
      temp <- load_corine(n1, union=TRUE, use_cache=TRUE, verbose=verbose)
      return(NULL)
    })

  ## Then cleanup:
  clc_cache <- file.path(hexscape_getOption("storage_folder"), "processed_data", "clc_by_code")
  unlink(list.files(clc_cache, full.names=TRUE), recursive=TRUE)

  invisible(NULL)
}

#' @rdname load_corine
#' @export
load_corine <- function(nuts_code, union=FALSE, use_cache=validate_corine_cache(), verbose=1L){

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
        if(length(attr(nc, "version"))==1L && attr(nc, "version") >= package_version("0.4.5")){
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
        nc <- extract_corine(nm, use_cache=use_cache, verbose=verbose)

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

## Manual list of NUTS1 codes that don't have Corine coverage:
nuts1_no_corine <- c("FRY")
