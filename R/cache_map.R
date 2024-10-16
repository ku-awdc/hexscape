cache_map <- function(nuts_code, year){

  qassert(nuts_code, "S1")
  qassert(year, "S1")

  ## Verify that maps are available:
  ddir <- hs_data_dir("gisco", year, create_subdir=FALSE)
  if( !file.exists(file.path(ddir, "raw_nuts.rqs")) || !file.exists(file.path(ddir, "raw_lau.rqs"))){
    stop("Maps data have not been downloaded:  see download_maps")
  }
  nuts <- qread(file.path(ddir, "raw_nuts.rqs"))
  lau <- qread(file.path(ddir, "raw_lau.rqs"))
  ## TODO: save within R package environment

  ## nuts_code should be a valid NUTS1 code:
  all_nuts1 <- nuts |> as_tibble() |> filter(LEVL_CODE==1) |> distinct(NUTS_ID) |> pull(NUTS_ID)
  if(!nuts_code %in% all_nuts1) stop("Provided code '", nuts_code, "' is not a valid NUTS1 code")

  ## filter to relevant NUTS1:
  nuts |>
    filter(str_sub(NUTS_ID, 1L, 3L) == nuts_code) ->
    nuts
  nnuts <- nrow(nuts)

  ## find matching lau:
  lau |>
    filter(CNTR_CODE == str_sub(nuts_code, 1L, 2L)) ->
    lau
  nlau <- nrow(lau)

  nuts3 <- nuts |> filter(LEVL_CODE==3)
  ## add corresponding nuts3:
  st_contains(
    nuts3 |> pull(geometry),
    lau |> pull(geometry) |> st_centroid(of_largest_polygon=TRUE),
    sparse=FALSE
  ) -> nuts3_contains_lau
  stopifnot(apply(nuts3_contains_lau,2L,sum)==1L)

  lau |>
    mutate(NUTS3 = nuts3[["NUTS_ID"]][apply(nuts3_contains_lau, 2, which)]) |>
    mutate(NUTS_ID = str_c(NUTS3, "L", LAU_ID)) |>
    mutate(LEVL_CODE = 4L, NUTS_NAME = LAU_NAME) ->
    lau

  bind_rows(
    nuts,
    lau |> select("NUTS_ID", "LEVL_CODE", "CNTR_CODE", "NUTS_NAME", "geometry")
  ) |>
    select(NUTS="NUTS_ID", Level="LEVL_CODE", Label="NUTS_NAME", "geometry") ->
    all

  stopifnot(nrow(all)==(nnuts+nlau), sum(all[["Level"]]==1L)==1L)

  ## Cache result:
  cdir <- hs_cache_dir("gisco", year, create_subdir=TRUE)
  qsave(all, file.path(ddir, str_c(nuts_code, ".rqs")))

  invisible(all)
}
