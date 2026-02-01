# Make an animated Christmas tree (gganimate GIF)

Generates a rotating 3D-ish point-cloud Christmas tree with falling
snow, then renders it as a GIF using gganimate + gifski.

## Usage

``` r
draw_xmas_tree_gif(
  style = 1,
  text = "Merry Christmas",
  text_x = 0,
  text_y = 0.6,
  text_size = 12,
  text_lineheight = 0.9,
  anno_text = NULL,
  anno_pos = "br",
  anno_size = 4,
  seed = 2025,
  n_frames = 90,
  fps = 50,
  width = 600,
  height = 600,
  out_gif = NULL,
  loop = TRUE,
  show = interactive()
)
```

## Arguments

- style:

  Integer index (1-10) or character name selecting a built-in color
  theme. Available styles: "classic", "warm cozy", "vibrant", "winter
  frost", "icy night", "monochrome", "pink", "pastel", "electric",
  "olive earth".

- text:

  Character scalar. Title text shown above the tree. Use `\\n` for
  multi-line text, e.g. `"Merry Christmas\\n2025"`.

- text_x, text_y:

  Numeric coordinates (in plot space) for the text annotation.

- text_size:

  Numeric text size passed to `annotate("text", ...)`.

- text_lineheight:

  Numeric line height for multi-line text.

- anno_text:

  Optional character scalar. Additional text annotation to display in a
  corner. If NULL (default), no corner text is displayed.

- anno_pos:

  Character scalar. Position for corner text: "tl" (top left), "tr" (top
  right), "bl" (bottom left), or "br" (bottom right) (default: "tr").

- anno_size:

  Numeric text size for the annotation text (default: 8).

- seed:

  Integer random seed for reproducibility.

- n_frames:

  Integer number of animation frames.

- fps:

  Integer frames per second.

- width, height:

  Integer output dimensions (pixels).

- out_gif:

  Optional character path. If provided, saves the GIF to this path.

- loop:

  Logical; whether the GIF should loop.

- show:

  Logical; if TRUE, prints the rendered animation in interactive
  sessions. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

An invisible list with elements:

- plot:

  A `ggplot` object (with `transition_manual`).

- animation:

  Animation object returned by
  [`gganimate::animate()`](https://gganimate.com/reference/animate.html).

- cfg:

  The resolved style configuration list.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- draw_xmas_tree_gif(style = 1, text = "Merry Christmas\\n2025")
# Or use style name:
res <- draw_xmas_tree_gif(style = "classic", text = "Merry Christmas\\n2025")
# Display explicitly (if show = FALSE):
print(res$animation)

# Save to file:
draw_xmas_tree_gif(
  style = "pink", 
  text = "Happy Holidays\\nWenbin", 
  out_gif = "xmas.gif", 
  show = FALSE
)
} # }
```
