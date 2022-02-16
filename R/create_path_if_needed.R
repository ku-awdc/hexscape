#' Title
#'
#' @param path_to_file
#'
#' @return `path_to_file`
#'
#' @examples
create_path_if_needed <- function(path_to_file) {
  fs::path_dir(path_to_file) %>%
    fs::dir_create(recurse = TRUE)
  path_to_file
}
