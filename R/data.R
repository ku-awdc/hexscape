#' NUTS codes
#'
#' A complete list of available NUTS codes (levels 0/1/2/3)
#'
#' @format ## `nuts_codes`
#' A data frame with 2,016 rows and 5 columns:
#' \describe{
#'   \item{Code}{Country code}
#'   \item{Country}{Country name}
#'   \item{Level}{NUTS level}
#'   \item{NUTS}{NUTS code}
#'   \item{Label}{NUTS label}
#' }
"nuts_codes"

#' Country codes and names
#'
#' A list of country codes and names that are potentially relevant to EU/NUTS
#'
#' @format ## `country_codes`
#' A data frame with 42 rows and 6 columns:
#' \describe{
#'   \item{Code}{Country code}
#'   \item{Country}{Country name in English}
#'   \item{Eurostat}{Is the country included in Eurostat data?}
#'   \item{Native}{Country name in native/local language}
#'   \item{French}{Country name in French}
#'   \item{German}{Country name in German}
#' }
#' @source <https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Tutorial:Country_codes_and_protocol_order#EU_and_euro_area_aggregates>
"country_codes"

#' Corine Land Cover codes
#'
#' A list of codes and explanations relating to Corine land use data
#'
#' @format ## `clc_codes`
#' A data frame with 44 rows and 5 columns:
#' \describe{
#'   \item{CLC}{Corine Land Cover code}
#'   \item{CLC_Label1}{Description level 1}
#'   \item{CLC_Label2}{Description level 2}
#'   \item{CLC_Label3}{Description level 3}
#'   \item{CLC_RGB}{Suggested RBG colour code for plotting}
#' }
"clc_codes"
