# Core optimizer: assign samples to groups to balance mean + SD (Gini loss)

Internal/core function that performs the random search and returns the
best grouping. No plotting and no file writing.

The objective minimized is: \$\$loss = Gini(group\\means) + lambda \*
Gini(group\\sds)\$\$

## Usage

``` r
balance_partition_core(
  score,
  group_sizes,
  lambda = 1,
  B = 50000,
  method = c("blocked_permute", "random_assign"),
  seed = NULL
)
```

## Arguments

- score:

  Numeric vector (length n). The scalar to balance.

- group_sizes:

  Integer vector of target group sizes. Must sum to `length(score)`.

- lambda:

  Numeric \>= 0. Weight on SD-balance term. Default is `1`.

- B:

  Integer \>= 1. Number of candidate partitions to evaluate.

- method:

  Character. One of `"blocked_permute"` or `"random_assign"`.

- seed:

  Optional integer seed.

## Value

A list with:

- `groups`: integer vector (length n) of group labels 1..K

- `loss`: best achieved loss

- `group_sizes`: integer vector of target sizes used
