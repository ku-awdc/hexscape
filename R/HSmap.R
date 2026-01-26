#' @importFrom S7 new_class

if(FALSE){
HSmap <- new_class("HSmap",
  properties = list(
    proj = class_character,
    date = class_Date,
    type = class_character,
    data = class_data.frame
  )
)

HSmap(data=tibble(
  a = 1
))
}
