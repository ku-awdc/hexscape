## TODO
# Assume homogenous grid
# Get direction based on carrying-present+1 prob
# Look at distribution of where they end up after 1 dispersal event
# Parameters are baseline hazard and distance effect from Weibull
# Translate vector to scalar distance
# Add in possibility for increased mortality with distance
# Animals don't stop if breeding capacity = 0
# Less likely to stop if there is insufficient breeding space

library(HexScape)
library(sf)
library(magrittr)

# Did you know about this?
Sys.setenv(`_R_USE_PIPEBIND_` = TRUE)
# Which enables:
data.frame(x=1:10, y=rnorm(10)) |> d => lm(y ~ x, data=d)
# Rather than the much uglier syntax:
data.frame(x=1:10, y=rnorm(10)) |> (\(d) lm(y ~ x, data=d))()


## Parameters

# Directional probabilities:
dir_probs <- c(forward=0.35, f_right=0.2, b_right=0.1, backward=0.05, b_left=0.1, f_left=0.2)

## Side-note:
# This doesn't work for this use case :(
# dir_probs |> sum() |> x => stopifnot(x==1)
# Or this:
# dir_probs |> sum() |> `==`(1) |> stopifnot()
# And this is ugly:
dir_probs |> sum() |> (\(x) stopifnot(x==1))()
# So maybe the magrittr pipe is not yet dead...
dir_probs |> sum() %>% `==`(1) %>% stopifnot()
dir_probs %>% sum() %>% `==`(1) %>% stopifnot()
# Or maybe there is a more pipe-friendly 
# way of doing this e.g. assertive package?!?

## Back to the real problem...

#' Width of hexagons:
hexwth <- 2


#' ## Settling hazard

#' This controls the rate at which the settling hazard changes with distance - >1=increasing, <1=decreasing, 1=constant
settle_rho <- 1.5
#' The mean settling displacement in km:
settle_mean_disp <- 10
#' What this looks like as a distribution:
settle_scale <- settle_mean_disp / gamma(1 + 1/settle_rho)
curve(dweibull(x, settle_rho, settle_scale), from=0, to=100)
curve(pweibull(x, settle_rho, settle_scale), from=0, to=100)

#' Convert from displacement to distance (number of movements) by accounting for
#' directional probabilities and hexagon width:
settle_mean_dist <- settle_mean_disp / (
					dir_probs["forward"]*hexwth*1 +
					dir_probs["f_right"]*hexwth*0.5 + 
					dir_probs["f_left"]*hexwth*0.5 + 
					dir_probs["b_right"]*hexwth*-0.5 + 
					dir_probs["b_left"]*hexwth*-0.5 + 
					dir_probs["backward"]*hexwth*-1
					) |> as.numeric()
# Convert to intercept on the hazard scale:					
settle_intercept <- (log(settle_mean_dist) - log(gamma(1 + 1/settle_rho))) * -settle_rho
# So this is what the settling hazard looks like:
movements <- 1:100
settle_hazard <- settle_rho * movements^(settle_rho-1) * exp(settle_intercept)
plot(movements, settle_hazard, type="l")
# You can see what happens to this by adjusting settle_rho to be <1, ==1, >1

## TODO: decreased settling hazard if:
# The proposed patch has no spare breeding capacity (sows) or spare sows (boars)
# The proposed patch has a zero carrying capacity

## TODO: mortality hazard
# Should have a similar shape to the settling hazard I guess

## Generate a homogenous landscape:
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

patches <- generate_patches(landscape, hex_width=hexwth) |>
	mutate(BreedingCapacity = floor(5*area))
neighbours <- generate_neighbours(patches, calculate_border=TRUE) %>%
	left_join(patches |> as_tibble() |> select(Neighbour=Index, NeighbourCapacity=BreedingCapacity), by="Neighbour")

# Identify the central patch:
patches |>
	as_tibble() |>
	arrange(abs(row-mean(row)), abs(col-mean(col))) |>
	slice(1) |>
	mutate(Central=TRUE) |>
	select(Index, Central) |>
	right_join(patches, by="Index") |>
	replace_na(list(Central=FALSE)) |>
	identity() ->
patches

ggplot(patches, aes(geometry=geometry, fill=BreedingCapacity)) +
	geom_sf() +
	geom_sf(data = patches |> filter(Central), fill="red")

start_patch <- patches |> filter(Central) |> pull(Index)

## Migrate

migrate_fun <- function(plot=TRUE){

	# A group first selects a direction based on capacity of immediate neighbours:
	neighbours |>
		filter(Index==start_patch) |>
		# TODO: this should take into account also the number of sows in each neighbouring patch (currently zero)
		mutate(Probs=NeighbourCapacity+1) |>
		slice_sample(n=1, weight_by=Probs) |>
		pull(Direction) ->
	direction
	
	# Then we align the directional probabilities with this direction:
	(function(x){
		# (This code is horrible)
		move_index <- (1:6)-which(x==levels(direction))
		move_index[move_index<0] <- move_index[move_index<0]+6
		move_index <- move_index+1
		move_probs <- dir_probs[move_index]
		names(move_probs) <- levels(direction)
		return(move_probs)
	})(direction) ->
	move_probs
	
	# Then loop over possible movements:
	current_patch <- start_patch
	n_moves <- 0
	history <- rep(NA_integer_, 50)
	repeat{
		# Pick neighbour to move to:
		neighbours |>
			filter(Index==current_patch) |>
			mutate(Probs=move_probs[Direction]*Border) |>
			slice_sample(n=1, weight_by=Probs) |>
			pull(Neighbour) |>
			identity() ->
		current_patch

		# Evaluate our new neighbourhood area:
		patches |>
			filter(Index %in% c(current_patch, neighbours |> filter(Index==current_patch) |> pull(Neighbour))) ->
		neighbourhood 
		
		# Determine if we want to settle in this area
		## NEW LOGIC: we could settle either here or in a neighbouring patch
		# Note that movements are discrete so we need to integrate:
		integrate(\(m) settle_rho * m^(settle_rho-1) * exp(settle_intercept), n_moves, n_moves+1)[["value"]] |>
			p => rbinom(1, 1, p) ->
		settling
		## TODO: there is probably a better way of doing this using the cumulative hazard?
		## TODO: account for carrying capacities of this and surrounding patches
		
		n_moves <- n_moves+1
		if(length(history)<n_moves) length(history) <- length(history)*2
		history[n_moves] <- current_patch
		
		if(!settling){
			next
		}
		
		# If settling then determine where in this area to settle:
		neighbourhood |>
			slice_sample(n=1) ->
		final_patch
		# TODO: this should be based on carrying capacity
		
		history[n_moves+1] <- final_patch[["Index"]]
		break
	}
	
	history <- history |> na.omit() |> as.numeric()
	
	if(plot){
		patches |>
			mutate(Visited = case_when(
				Central ~ "Start",
				Index == history[length(history)] ~ "End",
				Index %in% history ~ "Path",
				TRUE ~ "NotVisited"
			)) ->
		tpatches
		
		labs <- arrange(patches, Index)[history,] |> mutate(Label = 1:n())
		
		{
			ggplot(tpatches) +
			geom_sf(aes(geometry=geometry, fill=Visited)) +
			scale_fill_manual(values=c(Start="red", End="blue", Path="dark grey", NotVisited="light grey")) +
			geom_sf_text(aes(geometry=geometry, label=Label), labs) +
			ggtitle(str_c("Direction: ", as.character(direction)))
		} |> print()
	}
		
	
	return(list(history=history, direction=as.character(direction)))
}

migrate_fun()

# To get density map of where we end up:
pbapply::pblapply(1:1000, \(it){
	rr <- migrate_fun(plot=FALSE)
	data.frame(Iteration=it, Direction=rr[["direction"]], End=rr[["history"]][length(rr[["history"]])])
}) |>
	bind_rows() ->
results

results |>
	count(Direction, End) |>
	group_by(Direction) |>
	mutate(Proportion = n / sum(n)) |>
	ungroup() |>
	select(Index=End, Direction, Proportion) |>
	full_join(expand_grid(Index=unique(patches$Index), Direction=levels(direction)), by=c("Index", "Direction")) |>
	replace_na(list(Proportion=0)) |>
	full_join(patches, by="Index") ->
plotres

ggplot(plotres) +
	geom_sf(aes(geometry=geometry, fill=Proportion)) +
	facet_wrap(~Direction) +
	geom_sf(aes(geometry=geometry), patches |> filter(Central), fill="red")
ggsave("dispersal_pattern.pdf")
