#' Locations for `hexscape` data
#'
#' @param subfolder Either `gisco` or `corine`.
#' @param year Default to 2021.
#' @param create_subfolders
#'
#' @importFrom  tools R_user_dir
#' 
#' @seealso [hs_cache_dir()]
#' 
hs_data_dir <- function(subdir=c("gisco","corine"), year=c("2021"), create_subdir=TRUE){

  subdir <- match.arg(subdir)

  if(is.numeric(year)) year <- as.character(year)
  year <- match.arg(year)

  bpath <- R_user_dir("hexscape", "data")

  if(!dir.exists(bpath)){
    cat("Creating package data directory at '", bpath, "'\n", sep="")
    dir.create(bpath, recursive=TRUE)
  }

  if(create_subdir && !dir.exists(file.path(bpath, subdir, year))){
    dir.create(file.path(bpath, subdir, year), recursive=TRUE)
  }

  return(file.path(bpath, subdir, year))
}

## TODO: functions hs_data_backup and hs_data_restore
## TODO: add README.txt to cache and data folders
