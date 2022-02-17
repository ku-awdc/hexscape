### Simple example for Mossa

library("tidyverse")
library("sf")
library("HexScape")


# A somewhat complex landscape:
p1 <- rbind(c(0,0), c(10,0), c(30,20), c(20,40), c(10,40), c(0,0))
p2 <- rbind(c(10,10), c(10,20), c(20,20), c(10,10))
landscape <- st_sfc(st_polygon(list(p1,p2)))
ggplot(landscape) + geom_sf()

# Choose a reference point that will be the centre of patch 0,0
# (optional; also this need not be within the landscape):
ref <- st_point(c(0, 40))

# Generate patches:
patches <- generate_patches(landscape, hex_width=5, reference_point = ref, name_index=FALSE)
patches

# Each patch now has an r and q in axial coordinates:
ggplot(patches, aes(label=str_c(q, ",", r))) +
  geom_sf() +
  geom_sf_text()

# Note: r and q may not be unique to patch (if it is split by a coastline), but Index will be:
ggplot(patches, aes(label=Index)) +
  geom_sf() +
  geom_sf_text()

# e.g. Index 26 and 25:
patches %>% filter(Index %in% c(25, 26))

# Generate neighbours:
neighbours <- generate_neighbours(patches)
neighbours

# Check these are correct using patch 32 (cross-reference with plot):
patches %>% filter(Index==32)
neighbours %>% filter(Index==32)

# Note that Index 33 has 7 neighbours (two to the SW):
neighbours %>% filter(Index==33)

