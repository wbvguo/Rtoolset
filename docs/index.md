# Rtoolset

**Rtoolset** is a miscellaneous tool set for R programming and data
analysis. It provides functions for sample and feature-level operations,
data transformation, R programming helpers, and visualizations. For
example:

- **📊 Sample Operations**: Balanced partitioning of
  samples/observations
- **🔬 Feature Operations**: Selection and filtering of
  features/genes/variables
- **🔄 Data Transformation**: Commonly-used data transformation for
  omics data
- **🔧 General Utilities**: String matching, formatting, file
  management, package installation
- **🎮 Visualization & Fun**: Interactive 2048 game, animated GIFs,
  interesting visualizations

*and more to come…*

![Rtoolset logo](reference/figures/Rtoolset.png)

## Installation

You can install Rtoolset from
[GitHub](https://github.com/wbvguo/Rtoolset.git) with:

``` r
# Using pak (recommended)
install.packages("pak")
pak::pak("wbvguo/Rtoolset")

# Or using remotes
install.packages("remotes")
remotes::install_github("wbvguo/Rtoolset")
```

## Quick Start

``` r
library(Rtoolset)

# Example: Create an animated Christmas tree
draw_xmas_tree_gif_panel()
```

## Documentation

1.  **[Full Documentation
    Website](https://wbvguo.github.io/Rtoolset/)** - Browse all
    functions and articles (currently under construction)

2.  **Vignettes** - Detailed tutorials (available after installation):

    ``` r
    browseVignettes("Rtoolset")
    ```

3.  For function reference, use standard R help:

    ``` r
    ?draw_heart
    ```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This package is licensed under the MIT License. See the
[LICENSE](https://wbvguo.github.io/Rtoolset/LICENSE) file for details.
