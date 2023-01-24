#' Extract land use data for a specific country
#'
#' @param country_code
#' @param nuts_year
#'
#' @importFrom pbapply pblapply pbsapply
#' @importFrom sf st_read st_union st_layers st_transform st_crs st_intersects st_intersection
#'
#' @export
extract_corine <- function(country_code, refresh=FALSE, verbose=0L, ...){

  stopifnot(length(country_code)==1)

  storage_folder <- hexscape_getOption("storage_folder")
  clc <- extract_clc()

  savename <- file.path(storage_folder, "processed_data", str_c("corine_", country_code, ".rds"))
  if(!refresh && file.exists(savename)){
    if(verbose > 1L) cat("Returning cached corine data for ", country_code, "...\n", sep="")

    corine_sf <- readRDS(savename) %>%
      left_join(clc, by="CLC_CODE") %>%
      select(CNTR_CODE, NUTS_ID, CLC_CODE, CLC_LABEL1, CLC_LABEL2, CLC_LABEL3, everything())

    return(corine_sf)
  }

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
  get_corine_sf <- function(cc) {
    code <- codes[cc]
    if (verbose > 1L) {
      cat("Extracting code ", code, " ... ", sep = "")

      ## hack:
      retfun <- function(rv) {
        cat(
          round(cc / length(codes) * 100),
          "% complete after ",
          round(as.numeric(Sys.time() - st, units = "mins")),
          " minutes\n",
          sep = ""
        )
        return(rv)
      }
    } else{
      retfun <- function(rv)
        rv
    }

    ## First extract the code and filter to only those that intersect
    ## anything we are interested in:
    obj <- layers$name %>%
      `names<-`(., .) %>%
      as.list() %>%
      lapply(function(l)
        suppressWarnings(st_read(
          corine_path,
          query = str_c("SELECT * FROM ", l, " WHERE Code_18 = ", code),
          layer = l,
          quiet = TRUE
        ))) %>%
      `[`(sapply(., nrow) > 0) %>%
      lapply(function(x)
        x %>% `colnames<-`(., case_when(
          colnames(.) %in% c("Shape", "Layer") ~ colnames(.),
          TRUE ~ toupper(colnames(.))
        ))) %>%
      bind_rows()

    if (nrow(obj) == 0)
      return(retfun(NULL))

    obj <- obj %>%
      st_transform(st_crs(map)) %>%
      mutate(use = st_intersects(Shape, mapsf, sparse = FALSE)[, 1]) %>%
      filter(use) %>%
      select(-use)

    if (nrow(obj) == 0)
      return(retfun(NULL))

    ## Then make intersections with each of the NUTS3 areas we have:
    o2 <- map %>%
      split(.$NUTS_ID) %>%
      map(
        ~ obj %>%
          mutate(use = st_intersects(Shape, .x$geometry, sparse = FALSE)[, 1]) %>%
          filter(use) %>%
          select(-use) %>%
          mutate(
            Shape = st_intersection(Shape, .x$geometry),
            NUTS_ID = .x$NUTS_ID
          )
      ) %>%
      bind_rows()

    return(retfun(o2))
  }

  if(verbose > 0L) cat("Extracting land use data relating to ", length(codes), " CLC codes for ", country_code, "\n[This will take some time]\n", sep="")
  if (verbose == 1L) {
    corine_raw <- pblapply(seq_along(codes), get_corine_sf) %>%
      bind_rows() %>%
      select(CLC_CODE = CODE_18, everything())
  } else{
    corine_raw <- lapply(seq_along(codes), get_corine_sf) %>%
      bind_rows() %>%
      select(CLC_CODE = CODE_18, everything())
  }

  corine_sf <- corine_raw %>%
    left_join(clc, by = "CLC_CODE") %>%
    left_join(map %>% as_tibble() %>% select(NUTS_ID, CNTR_CODE, NUTS_NAME),
              by = "NUTS_ID") %>%
    select(CNTR_CODE, NUTS_ID, CLC_CODE, OBJECTID, AREA_HA, OBJECTID, Shape)

  if (verbose > 0L)
    cat("Saving results...\n", sep = "")
  saveRDS(corine_sf, savename, compress = TRUE)

  if (verbose > 0L)
    cat("Done\n", sep = "")


  corine_sf <- corine_sf %>%
    left_join(clc, by = "CLC_CODE") %>%
    select(CNTR_CODE,
           NUTS_ID,
           CLC_CODE,
           CLC_LABEL1,
           CLC_LABEL2,
           CLC_LABEL3,
           everything())

  return(corine_sf)
}
