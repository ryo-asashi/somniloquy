#' Construct a Union-Find Structure
#'
#' @description
#' Creates a mutable Union-Find (Disjoint Set Union) data structure using an environment.
#' It implements path compression and union by size for efficient group operations.
#'
#' @param n An integer specifying the number of elements in the Union-Find structure.
#'
#' @examples
#' uf <- unionfind(5)
#'
#' # Unite 1 and 2, 2 and 3
#' uf$unite(1, 2)
#' uf$unite(2, 3)
#'
#' # Are 1 and 3 in the same group?
#' uf$find(1) == uf$find(3) # TRUE
#'
#' # Are 1 and 4 in the same group?
#' uf$find(1) == uf$find(4) # FALSE
#' @returns
#' \code{unionfind()} returns an environment containing the following property and methods:
#' \describe{
#'   \item{\code{parent}}{An integer vector of size \code{n}. A positive value indicates the index of the parent node. A negative value indicates that the node is a root, and its absolute value represents the size of the group.}
#'   \item{\code{find(x)}}{A function that returns the root index of the group containing \code{x}.}
#'   \item{\code{unite(x, y)}}{A function that unites the groups containing \code{x} and \code{y}. Returns \code{TRUE} if the groups were successfully united, or \code{FALSE} if they were already in the same group.}
#' }
#'
#' @export
unionfind <- function(n) {
  uf <- new.env(parent = emptyenv())

  # [property] parent index (if positive) or group size wrt a root (if negative)
  uf$parent <- rep.int(-1L, n)

  # [method] find
  uf$find <- function(x) {
    root <- x
    while (uf$parent[root] > 0L) {
      root <- uf$parent[root]
    }

    curr <- x
    while (curr != root) {
      nxt <- uf$parent[curr]
      uf$parent[curr] <- root
      curr <- nxt
    }

    return(root)
  }

  # [method] unite
  uf$unite <- function(x, y) {
    rx <- uf$find(x)
    ry <- uf$find(y)
    if (rx == ry) return(FALSE)

    # Union by size: ensure rx has the larger size (i.e., more negative)
    if (uf$parent[rx] > uf$parent[ry]) {
      tmp <- rx
      rx <- ry
      ry <- tmp
    }

    uf$parent[rx] <- uf$parent[rx] + uf$parent[ry]
    uf$parent[ry] <- rx

    return(TRUE)
  }

  return(uf)
}
