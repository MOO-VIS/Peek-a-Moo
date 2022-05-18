
plot_network <- function(nodes, edges) {
  # throw error if no data available for date range
  if(nrow(edges) == 0 ) stop('No data available for this date range')
  
  visNetwork(nodes,
             edges,
             width = "100%", height = "800px"
  ) %>%
    visNodes(
      font = list(size = 30),
      shape = "circle",
      shadow = TRUE,
      borderWidth = 1,
      color = list(
        border = "darkgray",
        highlight = list(background = "orange", border = "darkred")
      )
    ) %>%
    visEdges(
      smooth = list(enabled = TRUE, type = "horizontal"),
      color = list(color = "#D3D3D3", highlight = "orange", hover = "#2B7CE9")
    )  %>% 
    visInteraction(hover = TRUE, 
                   tooltipDelay = 100, 
                   tooltipStay = 300, 
                   dragNodes = FALSE,
                   selectable = FALSE,
                   selectConnectedEdges = FALSE) %>%
    visIgraphLayout(layout = "layout_in_circle") %>%
    visPhysics(stabilization = FALSE)
}

# plot_network <- function(g, cow_id) {
#   g$layout <- layout_in_circle(g)
#   inc.edges <- incident(g,  V(g)[cow_id])
#   inc.nodes <- c(ends(g, inc.edges[edge_width >= 1], names = FALSE))
#   ecol <- rep("grey80", ecount(g))
#   ecol[inc.edges] <- "orange"
#   vcol <- rep("white", vcount(g))
#   vcol[V(g)[inc.nodes]] <- "#ff9d00"
#   vcol[V(g)[cow_id]] <- "gold"
#   plot(g,
#        vertex.color=vcol, 
#        vertex.label.cex = 0.6,
#        edge.color = ecol,
#        edge.curved = FALSE,
#        edge.width=E(g)$edge_width)
#   legend("bottom", legend=c('0%-90%','90%-100%'), 
#          col = 'steelblue', lty = 1, lwd = levels(as.factor(E(g)$edge_width)), 
#          bty = 'n', title = 'Quantile of the number of association', 
#          inset = -0.2, cex = 0.8)
# }

plot_network_disp <- function(nodes, edges) {
  # throw error if no data available for date range
  if(nrow(edges) == 0 ) stop('No data available for this date range')
  
  plot_network(nodes, edges) %>%
    visNodes(
      shape = "dot"
    ) %>%
    visEdges(
      arrows = list(to = list(enabled = TRUE, scaleFactor = 0.8))
    ) %>%
    visOptions(
      nodesIdSelection = list(enabled = TRUE, main = "Select Focused Cow"),
      highlightNearest = list(
        enabled = T,
        degree = 1,
        hideColor = "rgba(0,0,0,0)",
        labelOnly = TRUE
      )
    ) %>% 
    visInteraction(dragNodes = TRUE, 
                   multiselect = TRUE,
                   selectable = TRUE,
                   selectConnectedEdges = TRUE)
}


plot_network_disp_star <- function(nodes, edges) {
  # throw error if no data available for date range
  if(nrow(edges) == 0 ) stop('No data available for this date range')
  
  visNetwork(nodes,
             edges,
             width = "100%", height = "800px"
  ) %>%
    visNodes(
      font = list(size = 20),
      shape = "dot",
      shadow = TRUE,
      borderWidth = 2,
      color = list(hightlight = "#D2E5FF", highlight.border = "#2B7CE9")
    ) %>%
    visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5))) %>%
    visPhysics(stabilization = FALSE)
}


combine_edges <- function(x, from_date = NULL, to_date = NULL, threshold = 0.9) {

  # throw error if no data available for date range
  if(from_date > to_date) stop('No data available for this date range')
  
  # set defaults
  from_date <- from_date %||% -Inf
  to_date <- to_date %||% Inf
  
  # combine list into one long data frame
  edgelist <- x %>%
    purrr::map_df(adjacency_to_long, .id = "date") %>%
    dplyr::mutate(dplyr::across(date, as.Date)) %>%
    dplyr::rename(weight = value) %>%
    filter(
      date >= from_date,
      date <= to_date
      ) %>%
    group_by(from, to) %>%
    summarise(weight = sum(weight, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(weight_bins = cut(weight,
                             breaks = c(
                               min(weight),
                               quantile(weight, threshold),
                               max(weight)
                             ),
                             include.lowest = TRUE, ordered = TRUE
    )) %>%
    mutate(
      width = as.integer(weight_bins) - 1,
      color.opacity = case_when(
        width >= 1 ~ 1,
        width < 1 ~ 0
      )
    )
  
  # return the edgelist
  edgelist
}

# combine dataframe for displacement data and create edges list (displacement)
combine_replace_edges <- function(x,
                                  from_date = NULL,
                                  to_date = NULL,
                                  CD_min = NULL,
                                  CD_max = NULL,
                                  threshold = 0.9) {
  # throw error if no data available for date range
  if(from_date > to_date) stop('No data available for this date range')
  
  # throw error if no CD available for CD range
  if(CD_min > CD_max) stop('No CD available for this CD range')
  
  # set defaults
  from_date <- from_date %||% -Inf
  to_date <- to_date %||% Inf
  CD_min <- CD_min %||% 0
  CD_max <- CD_max %||% 1
  
  combo_df <- as.data.frame(x) %>%
    mutate(date = as.Date(date)) %>%
    rename(
      CD = occupied_bins_with_feed_percent,
      from = Actor_cow,
      to = Reactor_cow
    ) %>%
    select(date, from, to, CD) %>%
    filter(
      date >= from_date,
      date <= to_date,
      CD <= CD_max,
      CD >= CD_min
    ) %>%
    group_by(from, to) %>%
    summarise(weight = n()) %>%
    ungroup() %>%
    mutate(weight_bins = cut(weight,
                             breaks = c(
                               min(weight),
                               quantile(weight, threshold),
                               max(weight)
                             ),
                             include.lowest = TRUE, ordered = TRUE
    )) %>%
    mutate(
      width = as.integer(weight_bins) - 1,
      color.opacity = case_when(
        width >= 1 ~ 1,
        width < 1 ~ 0
      ),
      title = paste0("Displacements: ", weight)
    )
}

# combine dataframe for displacement data and create edges list (star version)
combine_replace_edges_star <- function(x,
                                       from_date = NULL,
                                       to_date = NULL,
                                       cow_id = NULL,
                                       CD_min = NULL,
                                       CD_max = NULL) {
  # throw error if no data available for date range
  if(from_date > to_date) stop('No data available for this date range')
  
  # throw error if no CD available for CD range
  if(CD_min > CD_max) stop('No CD available for this CD range')
  
  # set defaults
  from_date <- from_date %||% -Inf
  to_date <- to_date %||% Inf
  CD_min <- CD_min %||% 0
  CD_max <- CD_max %||% 1
  
  combo_df <- as.data.frame(x) %>%
    mutate(date = as.Date(date)) %>%
    rename(
      CD = occupied_bins_with_feed_percent,
      from = Actor_cow,
      to = Reactor_cow
    ) %>%
    select(date, from, to, CD) %>%
    filter(
      date >= from_date,
      date <= to_date,
      (from == cow_id) | (to == cow_id),
      CD <= CD_max,
      CD >= CD_min
    ) %>%
    group_by(from, to) %>%
    summarise(weight = n()) %>%
    ungroup() %>%
    mutate(type = case_when(
      from == cow_id ~ "actor",
      to == cow_id ~ "reactor"
    )) %>%
    arrange(from != cow_id)
}

# create nodes list from edges list
combine_nodes <- function(edges, deg) {
  # throw error if no data available for date range
  if(nrow(edges) == 0 ) stop('No data available for this date range')
  
  nodes <- data.frame(id = unique(c(
    edges$from,
    edges$to
  ))) %>% 
    mutate(
      size = unname(deg) / max(unname(deg)) * 40, # Node size
      label = id
    )
}

# create nodes list from edges list (displacement)
combine_replace_nodes <- function(edges, deg) {
  # throw error if no data available for date range
  if(nrow(edges) == 0 ) stop('No data available for this date range')
  
  nodes <- data.frame(id = unique(c(
    edges$from,
    edges$to
  ))) %>% 
    mutate(
      size = unname(deg) / max(unname(deg)) * 40, # Node size
      title = paste0("Cow: ", id, "<br>Different Associations: ", unname(deg))
    )
}

# create nodes list from edges list (star version)
combine_replace_nodes_star <- function(edges, cow_id = NULL) {
  # throw error if no data available for date range
  if(nrow(edges) == 0 ) stop('No data available for this date range')
  
  nodes_size_to <- edges %>%
    group_by(to) %>%
    summarise(deg = sum(weight)) %>%
    rename(id = to)
  
  nodes_size_from <- edges %>%
    group_by(from) %>%
    summarise(deg = sum(weight)) %>% 
    rename(id = from)
  
  nodes_size <- rbind(nodes_size_to, nodes_size_from) %>% 
    group_by(id) %>%
    summarise(deg = sum(deg))
  
  nodes <- data.frame(id = unique(c(
    edges$from,
    edges$to
  ))) %>%
    mutate(
      color.background = case_when(
        id == cow_id ~ "orange",
        id != cow_id ~ "#D2E5FF"
      ),
      color.border  = case_when(
        id == cow_id ~ "darkred",
        id != cow_id ~ "#2B7CE9"
      ),
      label = paste(id)
    ) %>% 
    left_join(nodes_size, by = "id") %>% 
    mutate(
      size = case_when(
        id == cow_id ~ 20,
        id != cow_id ~ log(deg + 1) * 10
      ),
      title = case_when(
        id == cow_id ~ paste0("Center Cow"),
        id != cow_id ~ paste0("Cow: ", id, "<br>Displacements with Center: ", deg),
      )
    )
}
# combine_data <- function(x, from_date = NULL, to_date = NULL){
#   
#   # set defaults
#   from_date <- from_date %||% -Inf
#   to_date   <- to_date %||% Inf
#   
#   # combine list into one long data frame
#   edgelist <- x %>%
#     purrr::map_df(adjacency_to_long, .id = "date") %>%
#     dplyr::mutate(dplyr::across(date, as.Date)) %>%
#     dplyr::rename(weight = value) %>%
#     filter(date >= from_date,
#            date <= to_date) %>%
#     group_by(from, to) %>%
#     summarise(weight = sum(weight, na.rm = TRUE)) %>%
#     ungroup() %>%
#     mutate(weight_bins = cut(weight, 
#                              breaks = c(min(weight), 
#                                         quantile(weight, 9 / 10), 
#                                         max(weight)), 
#                              include.lowest = TRUE, ordered = TRUE)) %>%
#     mutate(edge_width = as.integer(weight_bins) - 1)  %>%
#     select(from, to , weight, edge_width)
#   
#   # return the edgelist
#   edgelist 
# }



# # combine dataframe for displacement data
# combine_replace_data <- function(x, 
#                                  from_date = NULL, 
#                                  to_date = NULL,
#                                  cow_id = NULL, 
#                                  CD_min = NULL, 
#                                  CD_max = NULL){
# 
#   # set defaults
#   from_date <- from_date %||% -Inf
#   to_date   <- to_date %||% Inf
#   CD_min   <- CD_min %||% 0
#   CD_max   <- CD_max %||% 1
#   
# combo_df <- as.data.frame(x) %>%
#   mutate(date = as.Date(date)) %>%
#   rename(CD = occupied_bins_with_feed_percent,
#          from = Actor_cow,
#          to = Reactor_cow) %>%
#   select(date, from, to, CD) %>%
#   filter(date >= from_date,
#          date <= to_date,
#          (from == cow_id)|(to == cow_id),
#          CD <= CD_max,
#          CD >= CD_min) %>%
#   group_by(from, to) %>%
#   summarise(weight = n()) %>%
#   ungroup() %>%
#   mutate(type = case_when(
#     from == cow_id ~ 'actor',
#     to == cow_id ~ 'reactor'
#   ))
# }

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
  x %>% as.data.frame() %>%
    tibble::rownames_to_column("to") %>%
    tidyr::pivot_longer(-to, "from", "time") %>%
    dplyr::filter(value > 0)
}

# adjacency_to_long <- function(x, upper_only = FALSE) {
#   # check inputs
#   dn <- dimnames(x)
#   if (!inherits(x, "matrix")) {
#     stop("Input must be a matrix")
#   } else if (is.null(dn) || is.null(dn[[1]]) || is.null(dn[[2]])) {
#     stop("Input matrix must have named dimensions.")
#   } else if (!all.equal(dn[[1]], dn[[2]])) {
#     stop("Dimension names must match across both axes")
#   }
#   
#   # zero-out the lower triangle if needed
#   if (upper_only) {
#     x[lower.tri(x)] <- 0
#   }
#   
#   # pivot data to long
#   x %>%
#     as.data.frame %>%
#     tibble::rownames_to_column("to") %>%
#     tidyr::pivot_longer(-to, "from", "time") %>%
#     dplyr::filter(value > 0)
# }


`%||%` <- function(x, y) {
  if (is_null(x)) y else x
}

