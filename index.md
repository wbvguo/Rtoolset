# Rtoolset

<div style="margin-bottom: 2em; line-height: 1.6;">
<p style="font-size: 1.1em; margin-bottom: 1.5em; color: #333;">
<strong>Rtoolset</strong> is a comprehensive collection of miscellaneous tools and utilities for R programming and data analysis. It provides functions for data visualization, statistical analysis, genomics, file management, and more.
</p>

<div style="display: flex; gap: 30px; align-items: flex-start; margin-bottom: 2em;">
<div style="flex: 1; line-height: 1.8;">

<p style="margin-bottom: 1em; font-weight: 500; color: #444;">The package currently provides utility functions for:</p>

<ul style="margin: 0; padding-left: 1.5em; line-height: 1.9;">
<li style="margin-bottom: 0.8em;"><strong>🔧 Utilities</strong>: String matching, formatting, file management, package installation</li>
<li style="margin-bottom: 0.8em;"><strong>📊 Data Analysis</strong>: Balanced partitioning, statistical analysis, and some helper functions in omics analysis (e.g., RNA-seq and DNA methylation)</li>
<li style="margin-bottom: 0.8em;"><strong>🎮 Games and Fun visualizations</strong>: Interactive 2048 game, animated GIFs</li>
</ul>

<p style="margin-top: 1em; font-style: italic; color: #666;">and more to come...</p>

</div>
<div style="flex: 0 0 220px; text-align: center; padding-top: 1em;">

<img src="man/figures/Rtoolset.png" alt="Rtoolset logo" style="max-width: 100%; height: auto; display: block; margin: 0 auto;" />

</div>
</div>
</div>

---

## Installation

<p style="margin-bottom: 1em; line-height: 1.7;">
You can install the development version of Rtoolset from <a href="https://github.com/wbvguo/Rtoolset.git">GitHub</a> with:
</p>

<div style="margin: 1.5em 0;">

```r
# Using pak (recommended)
install.packages("pak")
pak::pak("wbvguo/Rtoolset")

# Or using remotes
install.packages("remotes")
remotes::install_github("wbvguo/Rtoolset")
```

</div>

---

## Quick Start

<div style="margin: 1.5em 0;">

```r
library(Rtoolset)

# Example: Create an animated Christmas tree
draw_xmas_tree_panel_gif()
```

</div>

---

## Documentation

<div style="line-height: 1.8;">

<ul style="margin: 0; padding-left: 1.5em;">
<li style="margin-bottom: 1em;">
<p style="margin-bottom: 0.5em;"><strong><a href="https://github.com/wbvguo/Rtoolset/docs/index.html">Full Documentation Website</a></strong> - Browse all functions and articles (currently under construction)</p>
</li>
<li style="margin-bottom: 1em;">
<p style="margin-bottom: 0.5em;"><strong>Vignettes</strong> - Detailed tutorials (available after installation):</p>
<div style="margin-left: 1em; margin-top: 0.5em;">

```r
browseVignettes("Rtoolset")
vignette("getting-started", package = "Rtoolset")
```

</div>
</li>
<li style="margin-bottom: 1em;">
<p style="margin-bottom: 0.5em;">For function reference, use standard R help:</p>
<div style="margin-left: 1em; margin-top: 0.5em;">

```r
?draw_heart
?balanced_partition
?filter_calcpm_dge
```

</div>
</li>
</ul>

</div>

---

## Contributing

<p style="line-height: 1.7; margin-bottom: 1em;">
Contributions are welcome! Please feel free to submit a Pull Request.
</p>

---

## License

<p style="line-height: 1.7;">
This package is licensed under the MIT License. See the <a href="LICENSE">LICENSE</a> file for details.
</p>
