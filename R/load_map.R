#' Extract a map for given country or NUTS area code
#'
#' @param nuts_codes a character vector of NUTS codes
#' @param level option to retrieve specific NUTS levels below matched nuts_codes
#' @param verbose
#'
#' @importFrom purrr quietly
#' @importFrom stringr str_c str_detect str_sub
#' @importFrom dplyr filter bind_rows pull
#' @importFrom rlang set_names
#' @importFrom checkmate qassert assert_numeric
#'
#' @export
load_map <- function(nuts_codes, level=NULL, year="2021", verbose=1L){

  all_codes <- all_nuts_codes(year=year)
  stopifnot(nuts_codes %in% all_codes[["NUTS"]])

  if(is.null(level)){
    # If level is NULL then use nuts_codes precisely as they are
    out_codes <- nuts_codes
  }else{
    # Otherwise use all lower-level matching NUTS/LAU as indicated:
    qassert(level, "N+")
    assert_numeric(level, lower=0, upper=4)

    # Filter levels:
    all_codes |>
      dplyr::filter(Level %in% level) |>
      dplyr::pull("NUTS") ->
      all_codes

    # Match specific codes:
    vapply(str_c("^", nuts_codes), function(p){
      str_detect(all_codes, p)
    }, logical(length(all_codes))) |>
      apply(1,any) ->
      keep
    out_codes <- all_codes[keep]
  }

  # Get needed NUTS0:
  out_codes |>
    str_sub(1L, 2L) |>
    unique() ->
    nuts0

  # Load cached maps:
  nuts0 |>
    set_names() |>
    map(function(cc){
      ## TODO: cache internally within the package environment
      ## to avoid subsequent calls to read_map for the same country
      ## and year
      mp <- read_map(cc, year)
      return(mp)
    }, .progress = verbose>1L) |>
    bind_rows() ->
    all_maps

  # Filter out result:
  all_maps |>
    filter(NUTS %in% out_codes) ->
    rv

  ## TODO: make output order match input order (and check lengths are the same
  ## when is.null(levels))

  class(rv) <- c("hs_gisco", class(rv))

  return(rv)

}
