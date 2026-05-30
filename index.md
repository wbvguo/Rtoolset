# Rtoolset

**Rtoolset** is a miscellaneous tool set for R programming and data analysis, providing utility functions for:

<div style="display: flex; gap: 30px; align-items: flex-start; margin-bottom: 1em;">
<div style="flex: 1;">

- 📊 [Sample Operations](articles/sample-partition.html): Balanced partition
- 🔬 [Feature Operations](articles/rnaseq-workflow.html): Selection and filtering
- 🔄 [Data Transformation](articles/dnam-workflow.html): Common transformations
- 💻 [R Programming](articles/utilities-guide.html): String matching, formatting, objects
- 🔧 [General Utilities](articles/utilities-guide.html): File management, package installation
- 🎮 [Visualization & Fun](articles/visualization.html): Interesting visualizations and games

*and more to come...*

</div>
<div style="flex: 0 0 160px; text-align: center;">

<img src="man/figures/Rtoolset.png" alt="Rtoolset logo" style="max-width: 100%; height: auto; display: block; margin: -1em auto 0 auto;" />

</div>
</div>


## Installation

You can install Rtoolset from [GitHub](https://github.com/wbvguo/Rtoolset.git) with:

```r
# Using pak (recommended)
install.packages("pak")
pak::pak("wbvguo/Rtoolset")

# Or using remotes
install.packages("remotes")
remotes::install_github("wbvguo/Rtoolset")
```


## Quick Start

```r
library(Rtoolset)

# Example: Create animated Christmas trees
draw_xmas_tree_gif_panel()
```
<img src="man/figures/xmas_panel.gif" alt="Xmas panel" width="800"/>


## Documentation
1. **[Full Documentation](https://wbvguo.github.io/Rtoolset/)** - Browse all functions and articles (currently under construction)

2. **Vignettes** - Detailed tutorials (available after installation):

    ```r
    browseVignettes("Rtoolset")
    ```

3. For function reference, use standard R help:

    ```r
    ?closestMatch
    ```
