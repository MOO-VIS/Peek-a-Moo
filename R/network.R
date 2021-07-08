library(tidygraph)

#' Plot a Network of Cows
#'
#' @param g A tidygraph object. Expects a weighted undirected graph.
#' @param conditions An expression denoting a set of filtering conditions for
#'   the edge list. Defaults `friendship_rank <= 3` to keep only the top three
#'   relationships per cow.
#' @param weight An unquoted name for the edge weight variable. Defaults to
#'   paired_lying_time.
#'
#' @return A visNetwork chart.
#' @export
#'
#' @examples
#' x <- HOBO$`paired lying total time`
#' g <- make_tidygraph(x)
#' plot_network(g)
plot_network <- function(g, conditions = (from_friendship_rank <= 1 | to_friendship_rank <= 1 | paired_lying_time > quantile(paired_lying_time, 0.95)),
                         weight = "paired_lying_time") {
  edges <- g %E>%
    as_tibble %>%
    dplyr::filter({{ conditions }}) %>%
    mutate(width = scales::rescale(!!sym(weight), to = c(1, 10)))
  nodes <- g %N>%
    as_tibble %>%
    tibble::rowid_to_column("id") %>%
    mutate(label = name,
           group = grp1,
           size = 10,
           size = scales::rescale(authority, to = c(3, 20)))
  visNetwork(nodes, edges, width = "100%", height = "800px") %>%
    visEdges(smooth = FALSE)
}


#' Combine list data into a single dataframe for a date range
#'
#' @param x A named list of adjacency matrices. The names must be dates
#'   in YYYY-MM-DD format.
#' @from_date A date up from which to include entries. Defaults to NULL (all
#'   dates).
#' @to_date A date up to which to include entries. Defaults to NULL (all
#'   dates).
#'
combine_data <- function(x, from_date = NULL, to_date = NULL){

  # set defaults
  from_date <- from_date %||% -Inf
  to_date   <- to_date %||% Inf

  # combine list into one long data frame
  x %>%
    purrr::map_df(adjacency_to_long, .id = "date") %>%
    dplyr::mutate(dplyr::across(date, as.Date)) %>%
    filter(date >= from_date,
           date <= to_date)
}

#' Create a Tidy Graph
#'
#' @param x A named list of adjacency matrices. The names must be dates
#'   in YYYY-MM-DD format.
#' @weight A unquoted name for the variable representing the weight of the
#'   edges in the graph. Defaults to `paired_lying_time` but may also be
#'   `n_interactions`.
#' @return A tidygraph object.
.make_tidygraph <- function(x, weight = "paired_lying_time") {

  combo_df <- combine_data(x)

  # reshape data into and edgelist
  edgelist <- combo_df %>%
    group_by(from, to) %>%
    summarise(n_interactions = n(),
              paired_lying_time = sum(value, na.rm = TRUE),
              .groups = "drop_last") %>%
    mutate(from_friendship_rank = row_number(desc(paired_lying_time))) %>%
    group_by(to) %>%
    mutate(to_friendship_rank = row_number(desc(paired_lying_time))) %>%
    mutate(key = map2_chr(to, from, ~paste0(sort(c(.x, .y)),
                                            collapse = "-"))) %>%
    ungroup() %>%
    distinct(key, .keep_all = TRUE) %>%
    select(-key)

  # create a simple tidygraph
  graw <- tidygraph::tbl_graph(edges = edgelist, directed = FALSE)

  # augment the graph with stats
  g <- graw %>%
    activate(nodes) %>%
    mutate(eigen = centrality_alpha(weights = !!sym(weight)),
           authority = centrality_authority(weights = !!sym(weight)),
           power = centrality_power(),
           grp1 = group_fast_greedy(weights = !!sym(weight)),
           grp2 = group_infomap(weights = !!sym(weight)),
           grp3 = group_leading_eigen(weights = !!sym(weight)),
           grp4 = group_louvain(weights = !!sym(weight)),
           grp5 = group_walktrap(weights = !!sym(weight), steps = 4))

  # return the graph
  g
}

#' @inherit .make_tidygraph
#' @export
#' @examples
#' x <- HOBO$`paired lying total time`
#' (g <- make_tidygraph(x))
make_tidygraph <- memoise::memoise(.make_tidygraph)

#' Turn An Adjacency Matrix Into a Long Tibble
#'
#' @param x A matrix with named dimensions.
#' @param upper_only A logical which indicates whether the lower triangular
#'   portion of the matrix should be zeroed out (removed) from the output.
#'   Defaults to FALSE.
#'
#' @return A tibble with columns `to` (matrix columns), `from` (matrix rows),
#'   and `value` (matrix entries). Automatically removes entries with zero.
#'
#' @examples
#' # create a demo matrix
#' set.seed(1)
#' A <- matrix(sample(0:2, 16, replace = TRUE),
#'             nrow = 4, dimnames = list(letters[1:4], letters[1:4]))
#'
#' # create a long table
#' adjacency_to_long(A)
#'
#' # create along table for the upper triangle only (appropriate) for
#' # undirected graphs.
#' adjacency_to_long(A, upper_only = TRUE)
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

#' Default value for `NULL`
#'
#' Attribution: This function is copied in its entirety from the `rlang`
#' package.
#'
#' This infix function makes it easy to replace `NULL`s with a default
#' value. It's inspired by the way that Ruby's or operation (`||`)
#' works.
#'
#' @param x,y If `x` is NULL, will return `y`; otherwise returns `x`.
#' @export
#' @name op-null-default
#' @examples
#' 1 %||% 2
#' NULL %||% 2
`%||%` <- function (x, y) {
  if (is_null(x)) y else x
}
