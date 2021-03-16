#' ---
#'
#' ---
#'
#'
#' ## Calculate distance between nodes in a graph

#' Health warning: extremely quickly written code, proceed at own risk

## TODO: Matt to put this in an R package with generate_landscape (plus the copernicus stuff)

#' FIXME: Depends on sourcing the script `generate_landspace.R`.
# source("notebooks/generate_landscape.R")



library("igraph")


#' Assume we have created sydjyl or patches using generate_landscape:
# patches <- sydjyl

edges <- patches %>%
  as_tibble() %>%
  select(From = Index, E, NE, SE, NW, W, SW) %>%
  gather(Direction, To, -From) %>%
  select(From, To, Direction) %>%
  filter(!is.na(To))
edges

graph <-
  graph_from_data_frame(
    edges,
    directed = TRUE,
    vertices = patches %>% as_tibble() %>% select(Index, area)
  )

patches_to_graph <- function(patches) {
  edges <- patches %>%
    as_tibble() %>%
    select(From = Index, E, NE, SE, NW, W, SW) %>%
    gather(Direction, To, -From) %>%
    select(From, To, Direction) %>%
    filter(!is.na(To))
  # edges

  graph <-
    graph_from_data_frame(
      edges,
      directed = TRUE,
      vertices = patches %>% as_tibble() %>% select(Index, area)
    )
  graph
}

# plot(graph)
distances <- shortest.paths(graph)

distances[1:10,1:10]


library("tidyverse")

pdat <- patches %>%
  arrange(Index) %>%
  mutate(Distance = distances[sample(1:nrow(distances), 1), ])

ggplot(pdat, aes(fill=Distance)) +
  geom_sf()

# Nodes that are not connected have Distance Inf (e.g. on islands):
pdat %>% filter(Distance == Inf)

#' ## Calculate dispersal queries
#'
#'

max_order <- 4;
#' Requirement: patches => graph


#'
#'
#' @note You can get the graph from patches description view [patches_to_grid].
#'
#'
minimal_neighbouring_orders <- function(graph, orders) {
  #TODO: orders should be positive integers,

  orders %>%

    # retrieve neighbours of provided `orders`
    map(function(order, graph) {
      list(order = order,
           center = V(graph)$name %>% as.integer(),
           ego = igraph::ego(graph = graph, order = order, mode = "out")  %>%
             map(as.integer)
      )
    }, graph = graph) %>%

    # these neighbours are not minimal -- we need to unpack this for
    # visualisation reasons
    #
    # the above would suffice for

    transpose() %>%
    as_tibble() %>%

    unnest(c(order, center, ego)) %>%
    unnest(ego) %>%

    pivot_wider(names_from = order,
                values_from = ego,
                names_prefix = "order_",
                values_fn = list) %>%
    # the above could be a nice way to provide these ego/neighbourhoods/zones
    # for the simulation model
    #

    # here we make the ego-sets become minimal sets
    # i.e. only the boundary of the neighbours of the given orders.
    pack(orders = starts_with("order")) %>%
    rowwise() %>%
    mutate(across(orders, function(order_sets) {
      order_sets %>% unlist(recursive = FALSE) %>% {
        map2(., lag(.), base::setdiff) %>%
          map(list) %>%
          as_tibble()
      }
    })) %>%
    unpack(orders) %>%
    identity()
}
#'
#'
#' Example with the node numbered 666.
#'
#'

uniform_neighbours_666 <- minimal_neighbouring_orders(
  graph = uniform_patches %>% patches_to_graph(),
  orders = 1:4
) %>%
  filter(center == 666) %>%
  pivot_longer(starts_with("order_"),
               names_to = c(NA, "order"),
               names_sep = "_",
               names_transform = list(order = as.integer),
               values_to = "neighbour") %>%
  unnest_longer(neighbour) %>%
  identity()

uniform_patches %>%
  mutate(Capacity = Capacity %>% cut.default(breaks = 2)) %>%  {
    ggplot(.) +
      geom_sf(aes(fill = Capacity)) +
      geom_sf(aes(fill = factor(order)),
              data = . %>%
                left_join(uniform_neighbours_666,
                  by = c("Index" = "neighbour")) %>%
                filter(center == 666,
                       Index != 666)) +

      geom_sf_label(aes(label = Index),
                    data = . %>%
                      filter(Index == 666)) +

      theme(aspect.ratio = 1) +
      NULL
  }
#'
#'
#'
#' Sydjylland
#'

sydjylland_888 <- minimal_neighbouring_orders(
  graph = sydjyl %>% patches_to_graph(),
  orders = 1:4
) %>%
  filter(center == 888) %>%
  pivot_longer(starts_with("order_"),
               names_to = c(NA, "order"),
               names_sep = "_",
               names_transform = list(order = as.integer),
               values_to = "neighbour") %>%
  unnest_longer(neighbour) %>%
  identity()

sydjyl %>%
  #TODO: redo this
  mutate(Capacity = area %>% cut.default(breaks = 2)) %>%  {
    ggplot(.) +
      geom_sf(aes(fill = Capacity)) +
      geom_sf(aes(fill = factor(order)),
              data = . %>%
                left_join(sydjylland_888,
                          by = c("Index" = "neighbour")) %>%
                filter(center == 888,
                       Index != 888)) +

      geom_sf_label(aes(label = Index),
                    data = . %>%
                      filter(Index == 888)) +

      theme(aspect.ratio = 1) +
      NULL
  }

#'
#'

centerpoint <- st_point(c(mean(xrange), mean(yrange)))

clustered_patches <- patches %>%
  mutate(Centrality = st_distance(centroid, centerpoint)[,1]) %>%
  mutate(Capacity = pmax(0, (100 - Centrality*2)) * area/hexarea)
# clustered_patches$centroid - centerpoint
st_is_empty()


clustered_neighbours <- minimal_neighbouring_orders(
  graph = clustered_patches %>% patches_to_graph(),
  orders = 1:4
) %>%
  filter(center == 888) %>%
  pivot_longer(starts_with("order_"),
               names_to = c(NA, "order"),
               names_sep = "_",
               names_transform = list(order = as.integer),
               values_to = "neighbour") %>%
  unnest_longer(neighbour) %>%
  identity()

clustered_patches %>%
  #TODO: redo this
  mutate(Capacity = area %>% cut.default(breaks = 2)) %>%  {
    ggplot(.) +
      geom_sf(aes(fill = Capacity)) +
      geom_sf(aes(fill = factor(order)),
              data = . %>%
                left_join(clustered_neighbours,
                          by = c("Index" = "neighbour")) %>%
                filter(center == 888,
                       Index != 888)) +

      geom_sf_label(aes(label = Index),
                    data = . %>%
                      filter(Index == 888)) +

      theme(aspect.ratio = 1) +
      NULL
  }
