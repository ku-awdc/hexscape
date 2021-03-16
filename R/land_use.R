land_use <- function(landscape){

  ## TODO: get pre-extracted land use per country from storage folder

  (load("/Users/matthewdenwood/Documents/Resources/Datasets/Corine/corine_sf.Rdata"))
  ## TODO: store as rds and one per country


  corine_dk <- corine_sf %>%
    filter(NUTS_ID %in% c("DK050","DK042","DK041","DK032"))

  ggplot(corine_dk) + geom_sf()

}
