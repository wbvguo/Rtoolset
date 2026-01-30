# Convert heart image to LEGO mosaic and extract build steps

Converts a heart image to a LEGO mosaic using brickr and extracts the
build steps/panels. The output can be used with
[`draw_heart_gif()`](https://wbvguo.github.io/Rtoolset/reference/draw_heart_gif.md)
to create an animated GIF.

## Usage

``` r
draw_heart_step(
  image_path = NULL,
  heart_image = NULL,
  num_steps = 6,
  width = 50,
  height = 50,
  show = FALSE
)
```

## Arguments

- image_path:

  Character scalar. Path to the PNG image file to convert. If NULL and
  `heart_image` is also NULL, a simple heart shape will be generated.

- heart_image:

  Optional. Can be a ggplot object (output from
  [`draw_heart()`](https://wbvguo.github.io/Rtoolset/reference/draw_heart.md))
  or a PNG image array. If provided, `image_path` is ignored.

- num_steps:

  Integer. Number of build steps for the instructions (default: 6).

- width, height:

  Integer. Dimensions for the LEGO mosaic (default: 50x50).

- show:

  Logical. If TRUE, displays the final mosaic plot. Defaults to `FALSE`
  (display is currently disabled due to ggplot2/brickr compatibility
  issues).

## Value

A list with elements:

- mosaic:

  The `brickr` mosaic object.

- build:

  The build instructions object.

- steps:

  List of ggplot objects for each build step.

## Note

The brickr package is available on GitHub and must be installed with
`remotes::install_github("ryantimpe/brickr")` before using this
function.

## Examples

``` r
if (FALSE) { # \dontrun{
# Using an existing image file
heart_steps <- draw_heart_step(image_path = "heart.png", num_steps = 6)

# Using a ggplot object from draw_heart()
heart_plot <- draw_heart()
heart_steps <- draw_heart_step(heart_image = heart_plot, num_steps = 6)

# Display the mosaic
print(brickr::build_mosaic(heart_steps$mosaic))
} # }
```
