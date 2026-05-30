# Omics Overview

This vignette is the landing page for omics-related utilities in
`Rtoolset`. It provides a short overview of the available helpers and
points to the more focused RNA-seq and DNA methylation tutorials.

## Overview

`Rtoolset` currently includes lightweight helpers for a few common
preprocessing tasks across omics workflows:

- filtering and normalizing RNA-seq count matrices
- log transformation and feature ranking
- converting DNA methylation beta values to M values

These functions are designed to plug into larger workflows built with
packages such as `edgeR` and `limma`.

## Available Tutorials

### RNA-seq Utilities

The RNA-seq tutorial covers:

- [`filter_calcpm_dge()`](https://wbvguo.github.io/Rtoolset/reference/filter_calcpm_dge.md)
  for filtering and CPM calculation
- [`log_transform()`](https://wbvguo.github.io/Rtoolset/reference/log_transform.md)
  for custom transformations
- [`get_top_var_mat()`](https://wbvguo.github.io/Rtoolset/reference/get_top_var_mat.md)
  for selecting highly variable features

See the [RNA-seq
Utilities](https://wbvguo.github.io/Rtoolset/articles/omics-rnaseq.md)
vignette for examples and a suggested preprocessing workflow.

### DNA Methylation Utilities

The DNA methylation tutorial focuses on:

- [`beta2M()`](https://wbvguo.github.io/Rtoolset/reference/beta2M.md)
  for converting beta values to M values

See the [DNA Methylation
Utilities](https://wbvguo.github.io/Rtoolset/articles/omics-methylation.md)
vignette for examples, the conversion formula, and downstream analysis
notes.

## Function Map

| Function                                                                                  | Typical use                                        |
|:------------------------------------------------------------------------------------------|:---------------------------------------------------|
| [`filter_calcpm_dge()`](https://wbvguo.github.io/Rtoolset/reference/filter_calcpm_dge.md) | RNA-seq filtering, normalization, and CPM workflow |
| [`log_transform()`](https://wbvguo.github.io/Rtoolset/reference/log_transform.md)         | Custom log transformation of count-like data       |
| [`get_top_var_mat()`](https://wbvguo.github.io/Rtoolset/reference/get_top_var_mat.md)     | Selecting highly variable features for exploration |
| [`beta2M()`](https://wbvguo.github.io/Rtoolset/reference/beta2M.md)                       | Converting methylation beta values to M values     |

## Choosing a Tutorial

- Start with [RNA-seq
  Utilities](https://wbvguo.github.io/Rtoolset/articles/omics-rnaseq.md)
  if you are working with raw count matrices.
- Start with [DNA Methylation
  Utilities](https://wbvguo.github.io/Rtoolset/articles/omics-methylation.md)
  if you are working with beta values from methylation assays.
- Return to this page when you want a compact overview of omics-related
  helpers in the package.

## See Also

- [Sample
  Partition](https://wbvguo.github.io/Rtoolset/articles/sample-partition.md)
- [Utilities
  Guide](https://wbvguo.github.io/Rtoolset/articles/utilities-guide.md)
- [Visualization](https://wbvguo.github.io/Rtoolset/articles/visualization.md)
