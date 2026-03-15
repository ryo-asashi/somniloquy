#' Build a Graph in CSR-like Format
#'
#' @description
#' Constructs a graph using a 1D-array-based Compressed Sparse Row (CSR) format.
#' This structure is highly optimized for fast traversal in algorithms like Dijkstra's.
#' Separating the graph construction from the path-finding algorithm avoids redundant processing when running algorithms multiple times on the same graph.
#'
#' @param n an integer specifying the number of nodes (vertices) in the graph.
#' @param u an integer vector of the starting nodes for each edge (1-indexed).
#' @param v an integer vector of the ending nodes for each edge (1-indexed).
#' @param w a numeric vector of edge weights (costs).
#' @param directed a logical value indicating whether the graph is directed. Defaults to \code{FALSE} (undirected).
#'
#' @examples
#' n <- 4
#' u <- c(1, 1, 2, 3)
#' v <- c(2, 3, 4, 4)
#' w <- c(2, 5, 1, 3)
#' graph <- build_graph(n, u, v, w)
#' @returns
#' \code{build_graph()} returns a list containing the internal graph representation (\code{n}, \code{head}, \code{to}, \code{weight}, \code{nxt}, and \code{alloc_size}).
#'
#'
#' @export
build_graph <- function(n, u, v, w, directed = FALSE) {
  m <- length(u)
  alloc_size <- if (directed) m else 2L * m

  head <- rep.int(0L, n)
  to <- integer(alloc_size)
  weight <- numeric(alloc_size)
  nxt <- integer(alloc_size)

  edge_idx <- 1L
  for (i in seq_len(m)) {
    ui <- u[i]
    vi <- v[i]
    wi <- w[i]

    to[edge_idx] <- vi
    weight[edge_idx] <- wi
    nxt[edge_idx] <- head[ui]
    head[ui] <- edge_idx
    edge_idx <- edge_idx + 1L

    if (!directed) {
      to[edge_idx] <- ui
      weight[edge_idx] <- wi
      nxt[edge_idx] <- head[vi]
      head[vi] <- edge_idx
      edge_idx <- edge_idx + 1L
    }
  }

  return(list(
    n = n,
    head = head,
    to = to,
    weight = weight,
    nxt = nxt,
    alloc_size = alloc_size
  ))
}

#' Dijkstra's Algorithm on a Pre-built Graph
#'
#' @description
#' Computes the shortest paths from a single source node to all other nodes
#' using a graph constructed by \code{build_graph}.
#'
#' @param graph a list representing the graph, returned by \code{build_graph}.
#' @param start_node an integer specifying the source node.
#' @param inf a numeric value representing infinity. Defaults to \code{1e15}.
#'
#' @examples
#' n <- 4
#' u <- c(1, 1, 2, 3)
#' v <- c(2, 3, 4, 4)
#' w <- c(2, 5, 1, 3)
#' graph <- build_graph(n, u, v, w)
#'
#' cost_from_1 <- dijkstra(graph, start_node = 1)
#' cost_from_4 <- dijkstra(graph, start_node = 4)
#' @returns
#' \code{dijkstra()} returns a numeric vector of length \code{graph$n} containing the shortest distances.
#'
#' @export
dijkstra <- function(graph, start_node, inf = 1e15) {
  # --- R高速化の要：リストの要素をローカル変数にアンパック ---
  # ループ内で graph$head 等とアクセスすると非常に遅くなるため、最初に展開します。
  n <- graph$n
  head <- graph$head
  to <- graph$to
  weight <- graph$weight
  nxt <- graph$nxt
  alloc_size <- graph$alloc_size

  cost <- rep.int(inf, n)
  cost[start_node] <- 0

  max_q <- alloc_size + 1L
  todo_cost <- numeric(max_q)
  todo_node <- integer(max_q)

  todo_cost[1L] <- 0
  todo_node[1L] <- start_node
  n_heap <- 1L

  while (n_heap > 0L) {
    cv_cost <- todo_cost[1L]
    cv_node <- todo_node[1L]

    last_cost <- todo_cost[n_heap]
    last_node <- todo_node[n_heap]
    n_heap <- n_heap - 1L

    if (n_heap > 0L) {
      e <- 1L
      while ((child <- e * 2L) <= n_heap) {
        if (child < n_heap && todo_cost[child + 1L] < todo_cost[child]) {
          child <- child + 1L
        }
        if (last_cost <= todo_cost[child]) break
        todo_cost[e] <- todo_cost[child]
        todo_node[e] <- todo_node[child]
        e <- child
      }
      todo_cost[e] <- last_cost
      todo_node[e] <- last_node
    }

    if (cost[cv_node] < cv_cost) next

    edge <- head[cv_node]
    while (edge > 0L) {
      w_node <- to[edge]
      tmp <- cv_cost + weight[edge]

      if (tmp < cost[w_node]) {
        cost[w_node] <- tmp

        n_heap <- n_heap + 1L
        e <- n_heap
        while (e > 1L) {
          p <- e %/% 2L
          if (todo_cost[p] <= tmp) break
          todo_cost[e] <- todo_cost[p]
          todo_node[e] <- todo_node[p]
          e <- p
        }
        todo_cost[e] <- tmp
        todo_node[e] <- w_node
      }
      edge <- nxt[edge]
    }
  }

  return(cost)
}
