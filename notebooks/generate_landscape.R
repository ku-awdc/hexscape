#' ---
#' title: Script convert an arbitrary shape into hexagons
#' date: 2020-02-12
#' author: Matt Denwood
#' ---

library(tidyverse)
library(sf)
library(pbapply)
theme_set(theme_light())

#' Matt TODO:
#'
#'	- [ ] Clean up and put into an R package (it could also be used for e.g. DigiVet)
#'	- [ ] Vectorise the pblapply part
#' 	- [ ] Break patches that are split by masking into separate patches, so that each polygon is contiguous
#'	- [ ] Use raster data (at a lower resolution) to inform breeding capacity
#'	- [ ] Randomise breeding capacity using spatial clustering more flexibly
#'	- [ ] Introduce blockage lines and reflect these in neighbours with negative indices

#' Mossa TODO:
#'
#' - [ ] Use the uniform_patches or clustered_patches to test your model
#' - [ ] Plot the boar density after e.g. 20 years using the hexagons given here
#' - [ ] Capacity can be anywhere between 0 (no breeding, max mortality) to 100 (max possible breeding, min mortality)
#' - [ ] All indices are currently positive but negative indices in NE-E-SE-SW-W-NW will indicate blocked edges


## Width of hexagons (flat to flat):
hexwth <- 2
# Corresponds to an area just less than 2x2km squares:
(hexarea <- sqrt(3)*hexwth^2/2)

# Function to get a vector of polygons to cover a specified landscape area:
make_polygon <- function(landscape, hexwth, calculate_border=FALSE, buffer_dist=1, min_prop = 0.01){

	stopifnot(is.numeric(hexwth) && length(hexwth)==1 && !is.na(hexwth))
	# Height (corner to corner):
	hexhgt <- 2*hexwth / 3^0.5
	# Side length:
	hexlth <- hexhgt/2
	# Max area:
	hexarea <- sqrt(3)*hexwth^2/2

	# Matrix of point pairs for centroid 0,0:
	genpoly <- function(x, y){
		hexpoints <- t(matrix(c(
				0, hexhgt/2,
				hexwth/2, hexlth/2,
				hexwth/2, -hexlth/2,
				0, -hexhgt/2,
				-hexwth/2, -hexlth/2,
				-hexwth/2, hexlth/2,
				0, hexhgt/2
				), nrow=2) + c(x,y))
		st_sfc(st_polygon(list(hexpoints)), crs=st_crs(landscape))
	}

	stopifnot(inherits(landscape, "sfc"))
	landscape <- st_union(landscape)
	bbox <- st_bbox(landscape)

	stopifnot(is.numeric(buffer_dist) && length(buffer_dist)==1 && buffer_dist > 0)
	stopifnot(is.numeric(min_prop) && length(min_prop)==1 && min_prop <= 1 && min_prop >= 0)
	
	# An over-estimate of the number of centroids needed:
	nx <- ceiling((bbox["xmax"]-bbox["xmin"]) / hexwth) +1
	ny <- ceiling((bbox["ymax"]-bbox["ymin"]) / (hexhgt + hexlth)) *2  +1
	cat("Creating", ny*nx, "hexagons...\n")
	
	expand_grid(row = seq(0,ny-1,by=1), col = seq(0,nx-1,by=1)) %>%
	  mutate(offset = as.logical(row %% 2 < 0.5)) %>%
	  mutate(x = bbox["xmin"] + case_when( !offset ~ hexwth*col, offset ~ hexwth/2 + hexwth*col)) %>%
	  mutate(y = bbox["ymin"] + (hexhgt+hexlth)*row/2) %>%
	  mutate(Index = 1:n()) %>%
		split(.$Index) %>%
		map_df( ~ .x %>% mutate(geometry = genpoly(x, y), centroid = st_sfc(st_point(c(x,y))))) %>%
		st_as_sf() %>%
		mutate(OK = st_intersects(geometry, landscape, sparse=FALSE)[,1]) %>%
		filter(OK) %>%
		mutate(geometry = st_intersection(geometry, landscape)) %>%
		select(Index, row, col, centroid, geometry) %>%
	  # We need to remove small ones here as otherwise some can't be cast to POLYGON below:
	  mutate(area = as.numeric(st_area(geometry), units="m")) %>%
	  filter(area >= min_prop * hexarea) ->
	patches
	# ggplot(patches, aes(fill=row%%2==1)) + geom_sf()
	
	## Split hexagons that are made discontinuous by the landscape and remove small ones:
	lapply(seq_len(nrow(patches)),
	       function(i) suppressWarnings(st_cast(patches[i,], to="POLYGON"))) %>%
	  bind_rows() %>%
	  mutate(area = as.numeric(st_area(geometry), units="m")) %>%
	  filter(area >= min_prop * hexarea) %>%
	  # Information is duplicated by combination of row and col:
	  # mutate(HexIndex = Index, Index = 1:n()) ->
	  mutate(Index = 1:n()) ->
	patches
	# ggplot(patches) + geom_sf()
	
	# Some patches are split e.g. 45,4 contains an island and part of the mainland:
	# patches %>% group_by(row,col) %>% mutate(N=n()) %>% filter(N>1)
	# ggplot(patches %>% filter(row %in% 3:5, col %in% 44:46), aes(label=str_c(col,",",row))) + geom_sf() + geom_sf_label()
	# Some are split into more than 2:
	# patches %>% group_by(row,col) %>% mutate(N=n()) %>% filter(N>2)
	# ggplot(patches %>% filter(row %in% 45:47, col %in% 46:48), aes(label=str_c(col,",",row))) + geom_sf() + geom_sf_label()
	
	## Work out nearest neighbours based on hexagon properties:

	cat("Calculating neighbours for", nrow(patches), "hexagons...\n")

	patches %>%
	  mutate(geometry = st_buffer(geometry, dist=buffer_dist)) %>%
	  select(Index, centroid, area, geometry) ->
	  neighbours

	if(!calculate_border){
	  
	## Neighbours method 1:  use st_join to see which patches and neighbours overlap:
	neighbours %>%
	  select(Neighbour = Index, nb_centroid=centroid, nb_area=area) %>%
	  st_join(neighbours, join=st_intersects) %>%
	  filter(Neighbour != Index) %>%
	  mutate(Border = NA_real_) ->
	neighbours
	# Note: this is fast for small # patches but may not scale as well?
	# and (more importantly) does not give us the length of the border
	
	}else{ # if !calculate_border 
	 
	## Neighbours method 2:  use st_intersection to get actual area (and therefore length) of border:
	patches %>%
	  mutate(geometry = st_buffer(geometry, dist=buffer_dist)) ->
	neighbours
	
	## Neighbours are limited to one of the following 8 options:
	expand_grid(row_adj=seq(-1,1,1), col_adj=seq(-1,1,1)) %>%
	  filter(row_adj!=0 | col_adj!=0) %>%
	  mutate(NeighbourNumber = 1:n()) %>%
	  split(.$NeighbourNumber) %>%
	  map_df( ~ bind_cols(.x, neighbours)) %>%
    mutate(row = row + row_adj, col = col + col_adj) %>%
    # Remove the 2 non-neighbours based on odd vs even row:
    mutate(offset = as.logical(row %% 2 < 0.5)) %>%
    mutate(using = case_when(
      row_adj == 0 ~ TRUE,
      col_adj == 0 ~ TRUE,
	    !offset & col_adj>0 ~ TRUE,
	    offset & col_adj<0 ~ TRUE,
	    TRUE ~ FALSE
	 )) %>%
	 filter(using) %>%
	 select(Neighbour=Index, row, col, nb_centroid=centroid, nb_geometry=geometry, nb_area=area) %>%
	 as_tibble() %>%
	 right_join(neighbours, by=c("row","col")) %>%
	  select(Index, Neighbour, area, centroid, geometry, nb_area, nb_centroid, nb_geometry) ->
  neighbours
	
#	rdpt <- patches %>% slice_sample(n=1)
#	ggplot() +
#	  geom_sf(data=rdpt, fill="red") +
#	  geom_sf(aes(geometry=nb_geometry), neighbours %>% filter(Index==rdpt$Index))
	
	# We have mostly 6 potential neighbours, but sometimes fewer (coastlines), 
	# and sometimes more (split patches)
	# neighbours %>% count(Index) %>% count(n)
	# NB: some patches may even have zero potential neighbours (but unlikely)
	
	## Now we have to calculate intersections a fairly slow way:
	neighbours %>%
	  mutate(complete_area = area > ((1-min_prop)*hexarea) & nb_area > ((1-min_prop)*hexarea)) ->
	neighbours
	
	## Shortcut for neighbours we know to be complete (i.e. the majority for simple landscapes):
	neighbours %>%
	  filter(complete_area) %>%
	  mutate(Border = hexlth) ->
	neighbours_complete_area
	
	## No shortcut for the rest:
	neighbours %>%
	  filter(!complete_area) ->
	neighbours_incomplete_area
	
	neighbours_incomplete_area$Intsct <- pbsapply(seq_len(nrow(neighbours_incomplete_area)),
	function(i){
	  x <<- neighbours_incomplete_area[i,]
	  intsct <- st_area(st_intersection(x$geometry, x$nb_geometry))
	  return(intsct)
	  
	  ### DEMO CODE:
	  
	  # Adjust for the buffer distance:
	  area <- as.numeric(st_area(intsct), units="m")
	  bdr <- (area - (buffer_dist*2)) / (buffer_dist*2)
	  
	  lstr <- st_union(st_intersection(x$boundary, x$nb_boundary))
	  # Not always true!!!
	  stopifnot(st_geometry_type(lstr)=="LINESTRING")
	  as.numeric(st_length(lstr), units="m")
	  
	  ggplot() +
	    geom_sf(data=patches %>% filter(Index %in% c(x$Index, x$Neighbour))) +
	    geom_sf(data=intsct, col="red") +
	    geom_sf(data=lstr, col="blue")
	  
	})
	
	neighbours_incomplete_area %>%
	  mutate(Border = (as.numeric(Intsct, units="m") - (buffer_dist*2)) / (buffer_dist*2)) %>%
	  filter(!is.na(Border)) %>%
	  select(-Intsct) %>%
	  bind_rows(neighbours_complete_area) %>%
	  filter(Border > min_prop*hexlth) ->
	neighbours
	
	# We still have mostly 6 neighbours, but sometimes fewer (coastlines), 
	# and more rarely more (split patches that aren't islands)
	# neighbours %>% count(Index) %>% count(n)
	# NB: some patches may even have zero potential neighbours (but unlikely)
	
	} # \if calculate_border 
	
	# Finally add the direction to these neighbours:
	st_coordinates(neighbours$nb_centroid - neighbours$centroid) %>%
	  as_tibble() %>%
	  bind_cols(neighbours %>% select(Index, Neighbour, Border, nb_area)) %>%
	  mutate(Direction = case_when(
	    abs(Y)<sqrt(.Machine$double.eps) & X > 0 ~ "E",
	    abs(Y)<sqrt(.Machine$double.eps) & X < 0 ~ "W",
	    Y > 0 & X > 0 ~ "NE",
	    Y < 0 & X > 0 ~ "SE",
	    Y > 0 & X < 0 ~ "NW",
	    Y < 0 & X < 0 ~ "SW"
	  )) %>%
	  mutate(Direction = factor(Direction, levels=c("NE","E","SE","SW","W","NW"))) %>%
	  select(Index, Neighbour, Border, Direction, nb_area) ->
	neighbours
	
	## Note: we can easily add "most significant" directional neighbours using:
	msnbs <- neighbours %>%
	  group_by(Index, Direction) %>%
	  arrange(desc(Border), desc(nb_area)) %>%
	  slice(1) %>%
	  ungroup() %>%
	  select(Index, Neighbour, Direction) %>%
	  spread(Direction, Neighbour)
	## BUT if not calculating borders then the "true" directional neighbour is chosen
	## according to the neighbour patch size - i.e. may not be correct!!
	
	# For now I will do this for back-compatibility BUT I am going to change the class to remove this
	# when I put it into a package (ie patch-level info will be separate to the network graph)
	patches <- full_join(patches, msnbs, by="Index")
	
	## Finally re-calculate the centroids:
	patches %>%
	  mutate(hex_centroid = centroid, centroid = st_centroid(geometry)) %>%
	  select(Index, row, col, centroid, hex_centroid, area, geometry, NE:NW) ->
	patches
	
	stopifnot(all(levels(neighbours$Direction) %in% names(patches)))
	
	class(patches) <- c("patches", class(patches))
	return(patches)
	
	
	## OLD CODE KEEPING FOR NOW IN CASE NEEDED
	if(FALSE) patches %>%
		split(.$Index) %>%
		pblapply(function(x) {

		  x <<- x
		  
			# The possible neighbours:
		  patches %>%
		    filter(abs(.data$row - x$row) <= 1, abs(.data$col - x$col) <= 1, .data$Index != x$Index) %>%
		    select(Neighbour = Index, geometry, centroid) ->
		  dirs
		  
		  # Bit of a hack because (for some reason) st_intersection sometimes gives a line and sometimes
		  # gives a polygon:
		  ## TODO: add a margin/padding to the geometries before calculating the intersection
		  dirs$Boundary <- sapply(seq_len(nrow(dirs)), function(i){
		    bbox <- st_bbox(st_intersection(x$geometry, dirs$geometry[i]))
		    #dst <- sqrt((bbox["ymax"]-bbox["ymin"])^2 + (bbox["xmax"]-bbox["xmin"])^2)
		    dst <- as.numeric(sqrt((bbox[4]-bbox[2])^2 + (bbox[3]-bbox[1])^2))
		  })
		  # dirs
		  # Some of these are twice as long as they should be:
		  # st_length(st_cast(st_intersection(x$geometry, dirs$geometry), "LINESTRING"))
		  # st_intersection(x$geometry, dirs$geometry)
		  #dirs %>%
		  #  filter(st_intersects(geometry, x$geometry, sparse=FALSE)[,1]) ->
		  #dirs
		  #dirs$Boundary <- as.numeric(st_length(st_intersection(x$geometry, dirs$geometry)), units="m")
		  
		  # Remove non-contiguous neighbours:
		  dirs %>% 
		    filter(!is.na(Boundary), Boundary > 0) ->
		  dirs
		  
		  if(nrow(dirs)==0) return(NULL)
		  
		  # dirs; ggplot(patches %>% filter(Index %in% c(x$Index, dirs$Neighbour)), aes(label=Index)) + geom_sf() + geom_sf_label()
		  # dirs; ggplot(patches %>% filter(Index %in% c(x$Index, dirs$Neighbour)), aes(label=str_c(row,",",col))) + geom_sf() + geom_sf_label()
		  
			# The direction to these neighbours:
			st_coordinates(dirs$centroid - x$centroid) %>%
				as_tibble() %>%
			  bind_cols(dirs %>% select(Neighbour, Boundary)) %>%
				mutate(Direction = case_when(
					abs(Y)<sqrt(.Machine$double.eps) & X > 0 ~ "E",
					abs(Y)<sqrt(.Machine$double.eps) & X < 0 ~ "W",
					Y > 0 & X > 0 ~ "NE",
					Y < 0 & X > 0 ~ "SE",
					Y > 0 & X < 0 ~ "NW",
					Y < 0 & X < 0 ~ "SW"
				), Index = x$Index) %>%
				select(Index, Neighbour, Boundary, Direction) ->
			dirs

			return(dirs)
		}) %>%
		bind_rows() ->
	neighbours

}


## Let's start with a square 50x50km landscape:
xrange <- c(0, 50)
yrange <- c(0, 50)
corners <- tribble(~x, ~y,
					xrange[1], yrange[1],
					xrange[2], yrange[1],
					xrange[2], yrange[2],
					xrange[1], yrange[2],
					xrange[1], yrange[1]
					)
landscape <- st_sfc(st_multipolygon(list(list(as.matrix(corners)))))

## And add our hexagons:
patches <- make_polygon(landscape, hexwth)
patches
ggplot(patches, aes(label=Index)) + geom_sf() + geom_sf_text()
patches <- make_polygon(landscape, hexwth, calculate_border = TRUE)
patches
ggplot(patches, aes(label=Index)) + geom_sf() + geom_sf_text()
stop()

if(FALSE){
	# ** sexy alert ** - this function works with any shapefile e.g. south jutland:
	library("eurostat")
	es <- get_eurostat_geospatial(output_class = "sf", resolution = "01", nuts_level = 'all', year = 2016) %>%
			st_transform(crs= 25832) %>%
			filter(CNTR_CODE == 'DK', LEVL_CODE == 3)  %>%
			filter(NUTS_ID == "DK032")

	sydjyl <- make_polygon(es$geometry, hexwth=2000, calculate_border = FALSE)

	ggplot(sydjyl, aes(fill=area)) + geom_sf()
	ggsave("sydjylland.pdf")

	# Neighbours now obey coastlines e.g. patch nr 353:
	sydjyl[353,] %>% select(NE:NW)
	ggplot(sydjyl) +
	  geom_sf() +
	  geom_sf(aes(geometry=centroid), col="red") +
	  geom_sf_text(aes(label=Index)) +
	  coord_sf(datum=st_crs(25832), xlim=c(540e3, 565e3), ylim=c(608e4, 610e4))
	
	# And we have more sensible row and columns:
	ggplot(sydjyl) +
	  geom_sf() +
	  geom_sf_text(aes(label=str_c(row, ",", col))) +
	  coord_sf(datum=st_crs(25832), xlim=c(540e3, 565e3), ylim=c(608e4, 610e4))
	
}

## Then add some carrying capacities:
uniform_patches <- patches %>%
	mutate(Capacity = 50 * area/hexarea)
ggplot(uniform_patches, aes(fill=Capacity)) + geom_sf()

random_patches <- patches %>%
	mutate(Capacity = runif(n(), 0, 100))
ggplot(random_patches, aes(fill=Capacity)) + geom_sf()


#' - [ ] Run Wild Boar Model(tm) for 5 iterations.
#'
#' Populate one patch in the middle
#'
#' Output: total wild boar for each patch for each year in each iteration.
#' 20 years
#'
#' Use ggplot/sf to show heatmap of the wildboar..
#'


centerpoint <- st_point(c(mean(xrange), mean(yrange)))
clustered_patches <- patches %>%
	mutate(Centrality = st_distance(centroid, centerpoint)[,1]) %>%
	mutate(Capacity = pmax(0, (100 - Centrality*2)) * area/hexarea)
ggplot(clustered_patches, aes(fill=Capacity)) + geom_sf()

clustered_patches %>%
  as_tibble() %>%
  bind_cols(.$centroid %>% st_coordinates() %>% as_tibble()) %>%
  # remove list-cols
  select(-centroid, -geometry) %>%
  # remove `Centrality` as this is used to make `Capacity`.
  select(-Centrality) %T>% {
    fs::dir_create(recurse = TRUE, "data/landscapes/")
  } %>%
  write_csv("data/landscapes/clustered_patches.csv")

anti_clustered_patches <- patches %>%
	mutate(Centrality = st_distance(centroid, centerpoint)[,1]) %>%
	mutate(Capacity = pmin(100, (Centrality*2)) * area/hexarea)

ggplot(anti_clustered_patches, aes(fill=Capacity)) + geom_sf()

# etc

## This should be the format needed for your model:
clustered_patches %>%
	as_tibble() %>%
	select(Index, Capacity, NE, E, SE, SW, W, NW)
