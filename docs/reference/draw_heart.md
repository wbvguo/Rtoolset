# Draw a 2D heart shape image

Creates a 2D heart shape image using ggplot2. The heart is generated
using parametric equations and can be saved to a PNG file. The saved
image can then be used with
[`draw_heart_step()`](https://wbvguo.github.io/Rtoolset/reference/draw_heart_step.md)
to create a LEGO mosaic.

## Usage

``` r
draw_heart(
  out_png = NULL,
  fill = "red",
  color = "firebrick",
  alpha = 0.9,
  show = interactive(),
  width = 4,
  height = 4
)
```

## Arguments

- out_png:

  Character scalar. Path where the heart image will be saved. If NULL,
  the image is not saved but the ggplot object is returned.

- fill:

  Character. Fill color for the heart (default: "red").

- color:

  Character. Border color for the heart (default: "firebrick").

- alpha:

  Numeric. Transparency level (0-1, default: 0.9).

- show:

  Logical. If TRUE, displays the heart plot. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

- width, height:

  Numeric. Width and height of the saved image in inches (default: 4).
  Only used when `out_png` is provided.

## Value

A `ggplot` object. If `out_png` is provided, the image is also saved to
disk.

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate and display a heart shape
heart <- draw_heart()
print(heart)

# Generate and save to file
heart <- draw_heart(out_png = "myheart.png")

# Customize colors
heart <- draw_heart(out_png = "pink_heart.png", fill = "pink", color = "hotpink")
} # }
```
