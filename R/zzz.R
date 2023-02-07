.onLoad <- function(lib, pkg)
{
  ## Set the storage folder if the global variable is found:
  sf <- Sys.getenv("HEXSCAPE_STORAGE")
  if(sf!="" && dir.exists(sf)){
    set_storage_folder(sf)
  }
}
