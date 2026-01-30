# Rtoolset

**Rtoolset** is a miscellaneous tool set for R programming and data
analysis, providing utility functions for:

- **📊 Sample Operations**: Balanced partitioning
- **🔬 Feature Operations**: Selection and filtering
- **🔄 Data Transformation**: Common transformations
- **💻 R Programming**: String matching, formatting, objects
- **🔧 General Utilities**: File management, package installation
- **🎮 Visualization & Fun**: Interesting visualizations and games

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
