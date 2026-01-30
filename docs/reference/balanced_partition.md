# Balanced partition of samples into groups using one numeric column (mean + SD)

User-facing wrapper around
[`balance_partition_core`](https://wbvguo.github.io/Rtoolset/reference/balance_partition_core.md).
Computes the best grouping using `score_col`, and optionally generates a
visualization and/or writes outputs to files.

## Usage

``` r
balanced_partition(
  data,
  score_col,
  K = NULL,
  group_sizes = NULL,
  id_col = NULL,
  lambda = 1,
  B = 50000,
  method = c("blocked_permute", "random_assign"),
  allow_unequal = TRUE,
  seed = NULL,
  output_dir = NULL,
  file_prefix = "balanced_partition",
  output_csv = TRUE,
  output_plot = FALSE
)
```

## Arguments

- data:

  A `data.frame` with samples in rows.

- score_col:

  Name of the numeric column to balance.

- K:

  Integer. Number of groups. Ignored if `group_sizes` is provided.

- group_sizes:

  Optional integer vector of target group sizes; must sum to
  `nrow(data)`.

- id_col:

  Optional sample ID column name. If `NULL`, uses row index.

- lambda:

  Numeric \>= 0. Weight on SD-balance term. Default `1`.

- B:

  Integer \>= 1. Number of random tries. Default `50000`.

- method:

  `"blocked_permute"` (default) or `"random_assign"`.

- allow_unequal:

  Logical. If TRUE and `n` is not divisible by `K`, group sizes differ
  by at most 1. Ignored if `group_sizes` is provided.

- seed:

  Optional integer seed for reproducibility.

- output_dir:

  Optional directory path. If provided, outputs are written here.

- file_prefix:

  Character. Prefix for output filenames.

- output_csv:

  Logical. If TRUE and `output_dir` is provided, write CSV of original
  data plus appended `.__group` and `.__score`.

- output_plot:

  Logical. If TRUE, generate a ggplot object and (if `output_dir` is
  provided) write a PNG.

## Value

A list with:

- `assignment`: data.frame with `sample_id`, `group`, `score`.

- `group_stats`: data.frame with per-group `n`, `mean`, `sd`.

- `loss`: best achieved loss.

- `group_sizes`: integer vector of target group sizes used.

- `plot`: ggplot object if `output_plot = TRUE`, else NULL.

- `files`: named list of written file paths if any were written, else
  NULL.

- `params`: list of parameters used.
