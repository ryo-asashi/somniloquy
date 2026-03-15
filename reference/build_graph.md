# Build a Graph in CSR-like Format

Constructs a graph using a 1D-array-based Compressed Sparse Row (CSR)
format. This structure is highly optimized for fast traversal in
algorithms like Dijkstra's. Separating the graph construction from the
path-finding algorithm avoids redundant processing when running
algorithms multiple times on the same graph.

## Usage

``` r
build_graph(n, u, v, w, directed = FALSE)
```

## Arguments

- n:

  an integer specifying the number of nodes (vertices) in the graph.

- u:

  an integer vector of the starting nodes for each edge (1-indexed).

- v:

  an integer vector of the ending nodes for each edge (1-indexed).

- w:

  a numeric vector of edge weights (costs).

- directed:

  a logical value indicating whether the graph is directed. Defaults to
  `FALSE` (undirected).

## Value

`build_graph()` returns a list containing the internal graph
representation (`n`, `head`, `to`, `weight`, `nxt`, and `alloc_size`).

## Examples

``` r
n <- 4
u <- c(1, 1, 2, 3)
v <- c(2, 3, 4, 4)
w <- c(2, 5, 1, 3)
graph <- build_graph(n, u, v, w)
```
