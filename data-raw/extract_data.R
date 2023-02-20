## Extract all country / nuts codes / nuts names

library("tidyverse")
library("eurostat")
library("readxl")

## Obtained 2023-02-17 from https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Tutorial:Country_codes_and_protocol_order#EU_and_euro_area_aggregates
allcountries <- read_excel("data-raw/country_codes.xlsx") |> arrange(Code)
stopifnot(all(table(allcountries[["Code"]])==1))

tt <- capture.output(map <- sf::st_read(file.path(Sys.getenv("HEXSCAPE_STORAGE"), "raw_data", "NUTS_RG_01M_2021_4326.shp")))
map |>
  as_tibble() |>
  select(Code = CNTR_CODE, Level = LEVL_CODE, NUTS = NUTS_ID, Label=NUTS_NAME) |>
  arrange(Level, Code, NUTS) |>
  left_join(
    allcountries |> select(Code, Country=English),
    by="Code"
  ) |>
  select(Code, Country, everything()) ->
nuts_codes

## Note: there is no easy way to extract which countries are covered by corine directly from the database

full_join(
  allcountries,
  nuts_codes |> distinct(Code, Eurostat=TRUE),
  by="Code"
) |>
  replace_na(list(Eurostat = FALSE)) |>
  select(Code, Country=English, Eurostat, everything()) ->
  country_codes

read_csv("data-raw/clc_legend.csv", col_types=cols(.default="c")) |>
  select(CLC=CLC_CODE, CLC_Label1 = LABEL1, CLC_Label2 = LABEL2, CLC_Label3 = LABEL3, CLC_RGB=RGB) ->
  clc_codes

names(clc_codes$CLC_RGB) = clc_codes$CLC

usethis::use_data(clc_codes, nuts_codes, country_codes, overwrite = TRUE)


## Not using:
eurostatcodes <- bind_rows(
  eu_countries |> select(Country=code, Name=name),
  ea_countries |> select(Country=code, Name=name),
  efta_countries |> select(Country=code, Name=name),
  eu_candidate_countries |> select(Country=code, Name=name)
) |> distinct(Country, Name) |>
  arrange(Country)

