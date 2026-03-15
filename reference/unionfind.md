# Construct a Union-Find Structure

Creates a mutable Union-Find (Disjoint Set Union) data structure using
an environment. It implements path compression and union by size for
efficient group operations.

## Usage

``` r
unionfind(n)
```

## Arguments

- n:

  An integer specifying the number of elements in the Union-Find
  structure.

## Value

`unionfind()` returns an environment containing the following property
and methods:

- `parent`:

  An integer vector of size `n`. A positive value indicates the index of
  the parent node. A negative value indicates that the node is a root,
  and its absolute value represents the size of the group.

- `find(x)`:

  A function that returns the root index of the group containing `x`.

- `unite(x, y)`:

  A function that unites the groups containing `x` and `y`. Returns
  `TRUE` if the groups were successfully united, or `FALSE` if they were
  already in the same group.

## Examples

``` r
uf <- unionfind(5)

# Unite 1 and 2, 2 and 3
uf$unite(1, 2)
#> [1] TRUE
uf$unite(2, 3)
#> [1] TRUE

# Are 1 and 3 in the same group?
uf$find(1) == uf$find(3) # TRUE
#> [1] TRUE

# Are 1 and 4 in the same group?
uf$find(1) == uf$find(4) # FALSE
#> [1] FALSE
```
