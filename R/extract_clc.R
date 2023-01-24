#' Title
#'
#' @return
#'
#' @export
extract_clc <- function() {
  storage_folder <- hexscape_getOption("storage_folder")

  clc_path <-
    file.path(storage_folder, "raw_data", "clc_legend.csv")
  stopifnot(file.exists(clc_path))

  ## Pull in CLC legend to break up codes and add code 999:
  clc <-
    read_csv(clc_path, col_types = cols(.default = col_character())) %>%
    bind_rows(
      tibble(
        GRID_CODE = "999",
        CLC_CODE = "999",
        LABEL1 = "Unknown",
        LABEL2 = "Unknown",
        LABEL3 = "Unknown",
        RGB = NA_character_
      )
    ) %>%
    select(
      CLC_CODE,
      CLC_LABEL1 = LABEL1,
      CLC_LABEL2 = LABEL2,
      CLC_LABEL3 = LABEL3
    )

  return(clc)
}
