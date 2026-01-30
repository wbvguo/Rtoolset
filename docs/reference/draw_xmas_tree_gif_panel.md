# Create a panel GIF from multiple style GIFs

Create a panel GIF from multiple style GIFs

## Usage

``` r
draw_xmas_tree_gif_panel(
  styles = 1:9,
  out_gif = "xmas_panel.gif",
  text_fun = function(i) paste0("Style ", i),
  ncol = 3,
  n_frames = 90,
  fps = 50,
  width = 300,
  height = 300,
  text_size = 11,
  anno_size = 3,
  loop = TRUE,
  tmp_dir = NULL,
  show = interactive()
)
```

## Arguments

- styles:

  Vector of style indices or names (default 1:9). Can be integer indices
  (1-10) or character style names (e.g.,
  `c("classic christmas", "warm cozy", "vibrant primary")`). See
  `draw_xmas_tree_gif` for available style names. The length of `styles`
  must be divisible by `ncol`.

- out_gif:

  Output GIF file path.

- text_fun:

  Function taking style (integer or character) and returning label text
  for that panel.

- ncol:

  Number of columns (default 3). Number of rows is calculated as
  `length(styles) / ncol`.

- n_frames:

  Frames per individual GIF (must match across).

- fps:

  Frames per second (must match across).

- width, height:

  Pixel size per tile (each individual GIF).

- text_size:

  Numeric text size for the main title text (default: 12).

- anno_size:

  Numeric text size for the annotation text (default: 4).

- loop:

  Logical; loop the final GIF.

- tmp_dir:

  Where to write temporary per-style GIFs.

- show:

  Logical; show final GIF in interactive sessions.

## Value

Invisibly returns the `magick-image` animation object.
