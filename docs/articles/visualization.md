# Visualization

Rtoolset provides several functions for creating beautiful
visualizations, including animated GIFs, heart shapes, and LEGO mosaics.
These tools are perfect for creating engaging presentations, social
media content, or fun visualizations.

## Overview

The visualization functions in Rtoolset include:

- **Animated Christmas Trees**: Create festive animated GIFs with
  customizable styles
- **Heart Shape Visualizations**: Generate 2D heart shapes and convert
  them to LEGO mosaics
- **Animated Build Sequences**: Create step-by-step animations showing
  LEGO build processes

All visualization functions support extensive customization including
colors, dimensions, text, and animation parameters.

## Animated Christmas Tree

### Basic Usage

Create an animated Christmas tree GIF:

``` r
library(Rtoolset)

# Create a simple animated tree
draw_xmas_tree_gif(
  style = "pink", 
  text = "Happy Holidays\nWenbin", 
  out_gif = "xmas.gif", 
  show = FALSE
)
```

### Panel-Style Animation

Create a panel-style animation showing multiple frames:

``` r
draw_xmas_tree_gif_panel()
```

### Available Styles

The
[`draw_xmas_tree_gif()`](https://wbvguo.github.io/Rtoolset/reference/draw_xmas_tree_gif.md)
function supports multiple built-in styles:

- `"classic"` or `1`: Classic Christmas colors
- `"warm cozy"` or `2`: Warm, cozy colors
- `"vibrant"` or `3`: Vibrant, bright colors
- `"winter frost"` or `4`: Cool, frosty colors
- `"icy night"` or `5`: Dark, icy colors
- `"monochrome"` or `6`: Black and white
- `"pink"` or `7`: Pink theme
- `"pastel"` or `8`: Pastel colors
- `"electric"` or `9`: Electric, neon colors
- `"olive earth"` or `10`: Earth tones

### Customization

You can customize various aspects of the tree:

``` r
draw_xmas_tree_gif(
  style = "classic",
  text = "Merry Christmas\n2025",
  text_x = 0,
  text_y = 0.6,
  text_size = 12,
  n_frames = 90,
  fps = 50,
  width = 600,
  height = 600,
  out_gif = "custom_tree.gif"
)
```

## Heart Shape Visualizations

### Creating a 2D Heart

Draw a simple 2D heart shape:

``` r
# Draw and save a heart
heart <- draw_heart(
  out_png = "heart.png",
  fill = "pink",
  color = "hotpink",
  alpha = 0.9,
  width = 4,
  height = 4
)

# Or just return the ggplot object
heart_plot <- draw_heart(out_png = NULL, fill = "red")
print(heart_plot)
```

### Converting to LEGO Mosaic

Convert a heart image to a LEGO mosaic with build steps:

``` r
# Using a ggplot object
heart_plot <- draw_heart(show = FALSE)
heart_steps <- draw_heart_step(
  heart_image = heart_plot,
  num_steps = 6,
  width = 50,
  height = 50
)

# Or using an image file
heart_steps <- draw_heart_step(
  image_path = "heart.png",
  num_steps = 6
)
```

**Note**: The
[`draw_heart_step()`](https://wbvguo.github.io/Rtoolset/reference/draw_heart_step.md)
function requires the `brickr` package, which must be installed from
GitHub:

``` r
remotes::install_github("ryantimpe/brickr")
```

### Creating Animated Build GIFs

Create an animated GIF showing the step-by-step build process:

``` r
# Create steps first
heart_steps <- draw_heart_step(
  heart_image = heart_plot,
  num_steps = 6
)

# Then create the GIF
draw_heart_gif(
  heart_steps = heart_steps,
  out_gif = "heart_animation.gif",
  interval = 0.5,
  show = FALSE
)

# Or do it all in one step
draw_heart_gif(
  image_path = "heart.png",
  out_gif = "heart_animation.gif",
  num_steps = 6,
  interval = 0.5
)
```

## Use Cases

### 1. Holiday Greetings

Create personalized holiday animations:

``` r
# Create a personalized Christmas tree
draw_xmas_tree_gif(
  style = "classic",
  text = "Happy Holidays\n2025",
  text_x = 0,
  text_y = 0.6,
  out_gif = "holiday_greeting.gif",
  show = FALSE
)
```

### 2. Social Media Content

Generate eye-catching visualizations for social media:

``` r
# Create a vibrant heart for Valentine's Day
draw_heart(
  out_png = "valentines_heart.png",
  fill = "red",
  color = "darkred",
  width = 8,
  height = 8
)
```

### 3. Educational Demonstrations

Show step-by-step build processes:

``` r
# Create an educational GIF showing build steps
heart_steps <- draw_heart_step(
  image_path = "heart.png",
  num_steps = 8,
  width = 50,
  height = 50
)

draw_heart_gif(
  heart_steps = heart_steps,
  out_gif = "build_demo.gif",
  interval = 0.3,
  show = FALSE
)
```

## Tips and Best Practices

1.  **File Formats**:
    - Heart images are saved as PNG files
    - Animated outputs are GIF files
    - Use appropriate dimensions for your needs
2.  **Performance**:
    - Larger dimensions and more frames increase rendering time
    - For quick previews, use smaller dimensions
    - For final outputs, use higher quality settings
    - Caching is enabled for faster vignette rebuilds
3.  **Customization**:
    - Experiment with different styles and colors
    - Adjust text positioning and size for your needs
    - Use `show = FALSE` when saving to files
4.  **LEGO Mosaics**:
    - More steps provide finer granularity but longer build sequences
    - Typical values are 4-8 steps
    - The mosaic dimensions affect the final resolution
    - Requires the `brickr` package from GitHub
5.  **Animation Settings**:
    - Higher FPS creates smoother animations but larger file sizes
    - More frames provide longer animations
    - Balance quality with file size for web use

## See Also

- Function reference:
  [`?draw_xmas_tree_gif`](https://wbvguo.github.io/Rtoolset/reference/draw_xmas_tree_gif.md),
  [`?draw_heart`](https://wbvguo.github.io/Rtoolset/reference/draw_heart.md),
  [`?draw_heart_step`](https://wbvguo.github.io/Rtoolset/reference/draw_heart_step.md)
- [Full documentation website](https://wbvguo.github.io/Rtoolset/)
