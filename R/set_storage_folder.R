#' Set storage folder for hexscape package
#'
#' @param folder
#'
#' @export
set_storage_folder <- function(folder){

  stopifnot(dir.exists(folder))
  hexscape_options(storage_folder = folder)

  if(!dir.exists(file.path(folder, "raw_data"))) dir.create(file.path(folder, "raw_data"))
  if(!dir.exists(file.path(folder, "processed_data"))) dir.create(file.path(folder, "processed_data"))
  if(!dir.exists(file.path(folder, "processed_data", "clc_by_code"))) dir.create(file.path(folder, "processed_data", "clc_by_code"))
  if(!dir.exists(file.path(folder, "landscapes"))) dir.create(file.path(folder, "landscapes"))

  ## TODO: look for the necessary files and print a message saying where to download stuff from

  ## TODO: check this is not already set:
  estfld <- file.path(folder, "eurostat_cache")
  if(!dir.exists(estfld)) dir.create(estfld)
  options(eurostat_cache_dir = estfld)

}
