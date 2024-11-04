#' @name load_corine
#' @title Extract land use data for a specific NUTS code area
#'
#' @param nuts_code
#' @param verbose
#' @param map
#' @param simplify_keep passed on to \code{\link[rmapshaper]{ms_simplify}}
#'
#' @importFrom pbapply pblapply pbsapply
#' @importFrom sf st_read st_union st_layers st_transform st_crs st_intersects st_intersection st_make_valid st_is_valid st_dimension st_as_sf st_centroid st_geometry
#' @importFrom rmapshaper ms_simplify
#' @importFrom units set_units
#' @importFrom qs qsave qread
#' @importFrom rlang set_names
#' @importFrom checkmate qassert assert_choice assert_number assert_names
#' @importFrom viridisLite viridis
#'

#' @rdname load_corine
#' @export
load_corine <- function(nuts_codes, clc_group=3L, simplify_keep=1.0, union=TRUE, use_cache=TRUE, verbose=1L, map_year=hs_year("map"), corine_year=hs_year("corine")){

  ## Check the nuts_code(s) are valid:
  qassert(nuts_codes, "S+")
  invalid <- which(!nuts_codes %in% all_nuts_codes()[["NUTS"]])
  if(length(invalid)>0L){
    stop("The following NUTS codes supplied are invalid: ", str_c(nuts_codes[invalid], collapse=", "))
  }

  years <- check_years(map=map_year, corine=corine_year)
  map_year <- years["map_year"]
  corine_year <- years["corine_year"]

  ## Convert nuts_codes to a list of maps and verify that all files are available:
  ddir <- hs_data_dir("corine", corine_year, create_subdir=FALSE)
  nuts_codes |>
    set_names() |>
    lapply(function(nc){
      ## Load map:
      map <- load_map(nc, map_year=map_year)
      ## Convert to NUTS1 (maybe multiple):
      anc <- all_nuts_codes(level=1L, pattern=str_sub(nc, 1L, min(3L, str_length(nc))))[["NUTS"]]
      ## Check all files are available:
      fp <- file.path(ddir, str_c(anc, ".rqs"))
      fp[!file.exists(file.path(ddir, str_c(anc, ".rqs")))] <- ""
      names(fp) <- anc
      list(corine_path=fp, map=map)
    }) ->
    corine_info

  corine_info |>
    lapply(function(x) x[["corine_path"]]) |>
    set_names(NULL) |>
    do.call("c", args=_) ->
    cpaths
  if(any(cpaths=="")){
    stop("Corine data for ", corine_year, " has not been downloaded for the following requested NUTS1 areas:\n  ", str_c(names(cpaths)[cpaths==""], collapse=", "))
  }

  ## clc_group can be 1:3 (where 3 is no aggregation), or a data frame:
  if(is.numeric(clc_group)){
    clc_group <- as.integer(clc_group)
    assert_choice(clc_group, c(1L,2L,3L))

    clc_codes |>
      mutate(Label = case_when(
        clc_group == 1L ~ .data$Label1,
        clc_group == 2L ~ .data$Label2,
        clc_group == 3L ~ .data$Label3,
      )) |>
      mutate(Code = str_sub(CLC, 1L, clc_group)) |>
      group_by(Code) |>
      mutate(Red = mean(Red)/255, Green = mean(Green)/255, Blue = mean(Blue)/255) |>
      ungroup() |>
      mutate(Colour = rgb(Red, Green, Blue)) |>
      select(CLC, Label, Code, Colour) ->
      clc_group

  }else if(is.data.frame(clc_group)){

    assert_names(names(clc_group), type="unique", must.include = c("CLC", "Label"))
    assert_names(clc_group[["CLC"]], type="unique", must.include = clc_codes[["CLC"]])

    if(!"Code" %in% names(clc_group)) clc_group[["Code"]] <- clc_group[["Label"]]
    if(!"Colour" %in% names(clc_group)){
      clc_group[["Colour"]] <- viridis(nrow(clc_group))
                                # scales::hue_pal()(nrow(clc_group))
    }
    qassert("S+", clc_group[["Colour"]])

  }else{
    stop("Invalid argument for 'clc_group'")
  }

  bind_rows(
    clc_group,
    tibble(
      CLC = NA_character_,
      Label = "Missing data",
      Code = NA_character_,
      Colour = "grey75"
    )
  ) ->
    clc_group

  qassert(simplify_keep, "N1[0.1,1]")
  qassert(union, "B1")
  qassert(use_cache, "B1")
  qassert(verbose, "N1[0,]")


  ## See if any relevant corine data has been cached:


  ## Loop over maps:
  corine_info |>
    lapply(function(nc){

      ## Load data, group, simplify, cache and return:
      lapply(nc[["corine_path"]], function(x){
        corine_raw <- qread(x)


      }) |>
        bind_rows() ->
        corine_raw

      ## Intersect with map provided:
      map <- nc[["map"]]

    })



  ##

  ## First do the aggregation:





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
