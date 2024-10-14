#' Title
#'
#' @param subfolder
#' @param year
#' @param create_subfolders
#'
#' @importFrom  tools R_user_dir
#'
#' @export
cache_dir <- function(subdir=c("raw","gisco","corine"), year=c("2021"), create_subdir=FALSE){

  subdir <- match.arg(subdir)

  if(is.numeric(year)) year <- as.character(year)
  year <- match.arg(year)

  bpath <- R_user_dir("hexscape", "data")

  if(!dir.exists(bpath)){
    cat("Creating package cache directory at '", bpath, "'\n", sep="")
    dir.create(bpath, recursive=TRUE)
  }

  if(create_subdir && !dir.exists(file.path(bpath, subdir, year))){
    dir.create(file.path(bpath, subdir, year), recursive=TRUE)
  }

  return(file.path(bpath, subdir, year))
}

## TODO: functions cache_backup and cache_restore
