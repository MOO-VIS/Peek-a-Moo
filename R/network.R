library(igraph)


plot_network <- function(g, cow_id) {
  g$layout <- layout_in_circle(g)
  inc.edges <- incident(g,  V(g)[cow_id])
  inc.nodes <- c(ends(g, inc.edges[edge_width >= 1], names = FALSE))
  ecol <- rep("grey80", ecount(g))
  ecol[inc.edges] <- "orange"
  vcol <- rep("white", vcount(g))
  vcol[V(g)[inc.nodes]] <- "#ff9d00"
  vcol[V(g)[cow_id]] <- "gold"
  plot(g,
       vertex.color=vcol, 
       vertex.label.cex = 0.6,
       edge.color = ecol,
       edge.curved = FALSE,
       edge.width=E(g)$edge_width)
  legend("bottom", legend=c('0%-90%','90%-100%'), 
         col = 'steelblue', lty = 1, lwd = levels(as.factor(E(g)$edge_width)), 
         bty = 'n', title = 'Quantile of the number of association', 
         inset = -0.2, cex = 0.8)
}

combine_data <- function(x, from_date = NULL, to_date = NULL){
  
  # set defaults
  from_date <- from_date %||% -Inf
  to_date   <- to_date %||% Inf
  
  # combine list into one long data frame
  edgelist <- x %>%
    purrr::map_df(adjacency_to_long, .id = "date") %>%
    dplyr::mutate(dplyr::across(date, as.Date)) %>%
    rename(weight = value) %>%
    filter(date >= from_date,
           date <= to_date) %>%
    group_by(from, to) %>%
    summarise(weight = sum(weight, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(weight_bins = cut(weight, 
                             breaks = c(min(weight), 
                                        quantile(weight, 9 / 10), 
                                        max(weight)), 
                             include.lowest = TRUE, ordered = TRUE)) %>%
    mutate(edge_width = as.integer(weight_bins) - 1)  %>%
    select(from, to , weight, edge_width)
  
  # return the edgelist
  edgelist 
}

# combine dataframe for displacement data
combine_replace_data <- function(x, 
                                 from_date = NULL, 
                                 to_date = NULL,
                                 cow_id = NULL, 
                                 CD_min = NULL, 
                                 CD_max = NULL){

  # set defaults
  from_date <- from_date %||% -Inf
  to_date   <- to_date %||% Inf
  CD_min   <- CD_min %||% 0
  CD_max   <- CD_max %||% 1
  
combo_df <- as.data.frame(x) %>%
  mutate(date = as.Date(date)) %>%
  rename(CD = occupied_bins_with_feed_percent,
         from = Actor_cow,
         to = Reactor_cow) %>%
  select(date, from, to, CD) %>%
  filter(date >= from_date,
         date <= to_date,
         (from == cow_id)|(to == cow_id),
         CD <= CD_max,
         CD >= CD_min) %>%
  group_by(from, to) %>%
  summarise(weight = n()) %>%
  ungroup() %>%
  mutate(type = case_when(
    from == cow_id ~ 'actor',
    to == cow_id ~ 'reactor'
  ))
}

.make_tidygraph <- function(x, edgelist = NULL, directed = FALSE) {
  
  edgelist <- edgelist %||% combine_data(x)
  g <- graph_from_data_frame(edgelist, directed = directed)
  
  # return the graph
  g
}


make_tidygraph <- memoise::memoise(.make_tidygraph)


adjacency_to_long <- function(x, upper_only = FALSE) {
  # check inputs
  dn <- dimnames(x)
  if (!inherits(x, "matrix")) {
    stop("Input must be a matrix")
  } else if (is.null(dn) || is.null(dn[[1]]) || is.null(dn[[2]])) {
    stop("Input matrix must have named dimensions.")
  } else if (!all.equal(dn[[1]], dn[[2]])) {
    stop("Dimension names must match across both axes")
  }
  
  # zero-out the lower triangle if needed
  if (upper_only) {
    x[lower.tri(x)] <- 0
  }
  
  # pivot data to long
  x %>%
    as.data.frame %>%
    tibble::rownames_to_column("to") %>%
    tidyr::pivot_longer(-to, "from", "time") %>%
    dplyr::filter(value > 0)
}


`%||%` <- function (x, y) {
  if (is_null(x)) y else x
}

