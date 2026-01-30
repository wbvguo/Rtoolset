# Create a LEGO-style animated heart GIF

Creates an animated GIF showing the step-by-step build process of a LEGO
heart mosaic from the build steps. The steps can be generated using
[`draw_heart_step()`](https://wbvguo.github.io/Rtoolset/reference/draw_heart_step.md).

## Usage

``` r
draw_heart_gif(
  heart_steps = NULL,
  image_path = NULL,
  heart_image = NULL,
  num_steps = 6,
  out_gif = "myheart.gif",
  width = 50,
  height = 50,
  interval = 0.5,
  show = interactive()
)
```

## Arguments

- heart_steps:

  Optional list. Output from
  [`draw_heart_step()`](https://wbvguo.github.io/Rtoolset/reference/draw_heart_step.md)
  containing the build steps. If provided, `image_path`, `heart_image`,
  `num_steps`, `width`, and `height` are ignored.

- image_path:

  Character scalar. Path to the PNG image file to convert. If NULL and
  `heart_image` is also NULL, a simple heart shape will be generated.
  Ignored if `heart_steps` is provided.

- heart_image:

  Optional. Can be a ggplot object (output from
  [`draw_heart()`](https://wbvguo.github.io/Rtoolset/reference/draw_heart.md))
  or a PNG image array. If provided, `image_path` is ignored. Ignored if
  `heart_steps` is provided.

- num_steps:

  Integer. Number of build steps in the animation (default: 6). Ignored
  if `heart_steps` is provided.

- out_gif:

  Character path. Output path for the GIF file. Default: "myheart.gif"
  in the current working directory.

- width, height:

  Integer. Dimensions for the LEGO mosaic (default: 50x50). Ignored if
  `heart_steps` is provided.

- interval:

  Numeric. Delay between frames in seconds (default: 0.5). Larger values
  create slower animations. Ignored if `heart_steps` is provided.

- show:

  Logical. If TRUE, displays the GIF after saving. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

Invisibly returns the `heart_steps` list (same structure as
[`draw_heart_step()`](https://wbvguo.github.io/Rtoolset/reference/draw_heart_step.md)).

## Examples

``` r
if (FALSE) { # \dontrun{
# Create steps and GIF in one step
draw_heart_gif(image_path = "heart.png", out_gif = "heart_animation.gif")

# Or create steps first, then GIF
heart_steps <- draw_heart_step(image_path = "heart.png", num_steps = 6)
draw_heart_gif(heart_steps = heart_steps, out_gif = "heart_animation.gif")

# Control animation speed (slower with larger interval)
draw_heart_gif(heart_steps = heart_steps, out_gif = "slow_heart.gif", interval = 1.0)

# Display the GIF after saving
draw_heart_gif(heart_steps = heart_steps, out_gif = "myheart.gif", show = TRUE)

# Full workflow
heart_plot <- draw_heart(out_png = "myheart.png")
heart_steps <- draw_heart_step(heart_image = heart_plot, num_steps = 6)
draw_heart_gif(heart_steps = heart_steps, out_gif = "myheart.gif", interval = 0.5, show = TRUE)
} # }
```
