
devtools::document()
devtools::load_all()

# download_maps()
load_map("DK031") |> autoplot()
load_map("DK031") |> autoplot() + aes(fill = Label)
load_map("DK031") |> autoplot() + scale_fill_manual()
