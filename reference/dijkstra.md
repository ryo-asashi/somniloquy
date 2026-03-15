# Dijkstra's Algorithm on a Pre-built Graph

Computes the shortest paths from a single source node to all other nodes
using a graph constructed by `build_graph`.

## Usage

``` r
dijkstra(graph, start_node, inf = 1e+15)
```

## Arguments

- graph:

  a list representing the graph, returned by `build_graph`.

- start_node:

  an integer specifying the source node.

- inf:

  a numeric value representing infinity. Defaults to `1e15`.

## Value

`dijkstra()` returns a numeric vector of length `graph$n` containing the
shortest distances.

## Examples

``` r
n <- 4
u <- c(1, 1, 2, 3)
v <- c(2, 3, 4, 4)
w <- c(2, 5, 1, 3)
graph <- build_graph(n, u, v, w)

cost_from_1 <- dijkstra(graph, start_node = 1)
cost_from_4 <- dijkstra(graph, start_node = 4)
```
