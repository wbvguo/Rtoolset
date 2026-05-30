# Package index

## Sample Operations

Functions that operate on samples/observations. See the [Sample
partition](https://wbvguo.github.io/Rtoolset/reference/articles/sample-partition.md)
vignette.

- [`balanced_partition()`](https://wbvguo.github.io/Rtoolset/reference/balanced_partition.md)
  : Balanced partition of samples into groups using one numeric column
  (mean + SD)
- [`balance_partition_core()`](https://wbvguo.github.io/Rtoolset/reference/balance_partition_core.md)
  : Core optimizer: assign samples to groups to balance mean + SD (Gini
  loss)

## Feature Operations

Functions that operate on features/genes/variables. See the [Omics
Overview](https://wbvguo.github.io/Rtoolset/reference/articles/omics-overview.md)
vignette.

- [`get_top_var_mat()`](https://wbvguo.github.io/Rtoolset/reference/get_top_var_mat.md)
  : get the the var_gene_list and topN (or top percent) most variable
  genes
- [`filter_calcpm_dge()`](https://wbvguo.github.io/Rtoolset/reference/filter_calcpm_dge.md)
  : filter countData by gene expression, perform cpm and log2(cpm+1)
  transformation

## Data Transformation

Functions that transform entire matrices/dataframes. See the [Omics
Overview](https://wbvguo.github.io/Rtoolset/reference/articles/omics-overview.md)
vignette.

- [`log_transform()`](https://wbvguo.github.io/Rtoolset/reference/log_transform.md)
  : perform log transformation with psuedo count
- [`beta2M()`](https://wbvguo.github.io/Rtoolset/reference/beta2M.md) :
  Convert beta values to M values

## Utilities

Helper functions for common tasks. See the [Utilities
Guide](https://wbvguo.github.io/Rtoolset/reference/articles/utilities-guide.md)
vignette.

- [`closestMatch()`](https://wbvguo.github.io/Rtoolset/reference/closestMatch.md)
  : find the closest match of a string in a vector of strings
- [`print_pval()`](https://wbvguo.github.io/Rtoolset/reference/print_pval.md)
  : print p-values in a more readable format
- [`vec2print()`](https://wbvguo.github.io/Rtoolset/reference/vec2print.md)
  : print a vector in a copy-paste friendly format
- [`mkdir()`](https://wbvguo.github.io/Rtoolset/reference/mkdir.md) :
  create a directory if it does not exist
- [`save2pdf()`](https://wbvguo.github.io/Rtoolset/reference/save2pdf.md)
  : save a plot to a PDF file
- [`extract_params()`](https://wbvguo.github.io/Rtoolset/reference/extract_params.md)
  : extract and assign default function arguments to the global
  environment
- [`createNamedList()`](https://wbvguo.github.io/Rtoolset/reference/createNamedList.md)
  : Create a named list
- [`install_packages()`](https://wbvguo.github.io/Rtoolset/reference/install_packages.md)
  : Install packages from CRAN, Bioconductor, or GitHub
- [`check_packages()`](https://wbvguo.github.io/Rtoolset/reference/check_packages.md)
  : Check which packages are not installed

## Visualization & Fun

Games and visualizations. See the
[Visualization](https://wbvguo.github.io/Rtoolset/reference/articles/visualization.md)
vignette.

- [`play_2048()`](https://wbvguo.github.io/Rtoolset/reference/play_2048.md)
  : Play the 2048 game in the R console
- [`draw_heart()`](https://wbvguo.github.io/Rtoolset/reference/draw_heart.md)
  : Draw a 2D heart shape image
- [`draw_heart_step()`](https://wbvguo.github.io/Rtoolset/reference/draw_heart_step.md)
  : Convert heart image to LEGO mosaic and extract build steps
- [`draw_heart_gif()`](https://wbvguo.github.io/Rtoolset/reference/draw_heart_gif.md)
  : Create a LEGO-style animated heart GIF
- [`draw_xmas_tree_gif()`](https://wbvguo.github.io/Rtoolset/reference/draw_xmas_tree_gif.md)
  : Make an animated Christmas tree (gganimate GIF)
- [`draw_xmas_tree_gif_panel()`](https://wbvguo.github.io/Rtoolset/reference/draw_xmas_tree_gif_panel.md)
  : Create a panel GIF from multiple style GIFs
