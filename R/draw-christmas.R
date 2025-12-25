#' Make an animated Christmas tree (gganimate GIF)
#'
#' Generates a rotating 3D-ish point-cloud Christmas tree with falling snow,
#' then renders it as a GIF using \pkg{gganimate} + \pkg{gifski}.
#'
#' @param style Integer index (1-10) or character name selecting a built-in color theme.
#'   Available styles: "classic", "warm cozy", "vibrant", "winter frost", "icy night",
#'   "monochrome", "pink", "pastel", "electric", "olive earth".
#' @param text Character scalar. Title text shown above the tree.
#'   Use `\\n` for multi-line text, e.g. `"Merry Christmas\\n2025"`.
#' @param text_x,text_y Numeric coordinates (in plot space) for the text annotation.
#' @param text_size Numeric text size passed to `annotate("text", ...)`.
#' @param text_lineheight Numeric line height for multi-line text.
#' @param anno_text Optional character scalar. Additional text annotation to display in a corner.
#'   If NULL (default), no corner text is displayed.
#' @param anno_pos Character scalar. Position for corner text: "tl" (top left), "tr" (top right),
#'   "bl" (bottom left), or "br" (bottom right) (default: "tr").
#' @param anno_size Numeric text size for the annotation text (default: 8).
#' @param seed Integer random seed for reproducibility.
#' @param n_frames Integer number of animation frames.
#' @param fps Integer frames per second.
#' @param width,height Integer output dimensions (pixels).
#' @param out_gif Optional character path. If provided, saves the GIF to this path.
#' @param loop Logical; whether the GIF should loop.
#' @param show Logical; if TRUE, prints the rendered animation in interactive sessions.
#'   Defaults to `interactive()`.
#'
#' @return An invisible list with elements:
#' \describe{
#'   \item{plot}{A `ggplot` object (with `transition_manual`).}
#'   \item{animation}{Animation object returned by `gganimate::animate()`.}
#'   \item{cfg}{The resolved style configuration list.}
#' }
#'
#' @examples
#' \dontrun{
#' res <- draw_xmas_tree_gif(style = 1, text = "Merry Christmas\\n2025")
#' # Or use style name:
#' res <- draw_xmas_tree_gif(style = "classic", text = "Merry Christmas\\n2025")
#' # Display explicitly (if show = FALSE):
#' print(res$animation)
#'
#' # Save to file:
#' draw_xmas_tree_gif(style = "pink", text = "Happy Holidays\\nWenbin", out_gif = "xmas.gif", show = FALSE)
#' }
#'
#' @export
draw_xmas_tree_gif <- function(
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
) {
  # ---- Package dependency checks (package-friendly) ----
  # In a real package, list these in DESCRIPTION (Imports) and call via ::.
  .require_pkg <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required but not installed.", pkg), call. = FALSE)
    }
  }
  pkgs <- c("ggplot2", "gganimate", "dplyr", "tidyr", "showtext", "sysfonts", "gifski")
  invisible(lapply(pkgs, .require_pkg))

  # ---- Styles ----
  styles <- list(
    "classic christmas" = list(
      bg_col = "#0B3D0B", tree_cols = c("#145414", "#006400"),
      trunk_col = "#3E2723", decor_cols = c("#FFD700", "#CCA43B", "#8B0000"),
      star_col = "#FFD700", text_col = "#FFD700", snow_col = "white",
      ribbon = TRUE, ribbon_col = "#D4AF37", ribbon_width = 1.5
    ),
    "warm cozy" = list(
      bg_col = "#2A1B17", tree_cols = c("#355E3B", "#4F7942"),
      trunk_col = "#5A3E2B", decor_cols = c("#D4AF37", "#B22222"),  
      star_col = "#FFD966", text_col = "#FFD966", snow_col = "#FFF5E1",
      ribbon = TRUE, ribbon_col = "#B22222", ribbon_width = 1.2
    ),
    "vibrant primary" = list(
      bg_col = "#101010", tree_cols = c("#006400", "#228B22"),
      trunk_col = "#4F3A2A", decor_cols = c("#FF0000", "#00FF00", "#FFFF00"),
      star_col = "#FFD700", text_col = "#FF6347", snow_col = "white",
      ribbon = TRUE, ribbon_col = "#C0C0C0", ribbon_width = 1.0
    ),
    "winter frost" = list(
      bg_col = "#F0F2F5", tree_cols = c("#2F4F4F", "#5F9EA0", "#708090"),
      trunk_col = "#696969", decor_cols = c("#B0C4DE", "#FFFAF0", "#E0FFFF"),
      star_col = "#B0C4DE", text_col = "#708090", snow_col = "#87CEFA",
      ribbon = TRUE, ribbon_col = "#B0C4DE", ribbon_width = 1.5
    ),
    "icy night" = list(
      bg_col = "#0E1A24", tree_cols = c("#C7D3DD", "#E6EEF4"),
      trunk_col = "#4A3F35", decor_cols = c("#FFFFFF", "#AFCBFF"),
      star_col = "#E8F1FF", text_col = "#E8F1FF", snow_col = "#FFFFFF",
      ribbon = FALSE, ribbon_col = NA, ribbon_width = 0
    ),
    "monochrome" = list(
      bg_col = "#000000", tree_cols = c("#111111", "#050505"),
      trunk_col = "#222222", decor_cols = c("#FFFFFF", "#FFFFE0"),
      star_col = "#FFFFFF", text_col = "#FFFFFF", snow_col = "gray30",
      ribbon = TRUE, ribbon_col = "#FFFFFF", ribbon_width = 0.8
    ),
    "pink romance" = list(
      bg_col = "#1F0F12", tree_cols = c("#D87093", "#FF69B4", "#FFB6C1"),
      trunk_col = "#4A3728", decor_cols = c("#FFFFFF", "#FFD1DC", "#FFB7C5"),
      star_col = "#FFD1DC", text_col = "#FFE4E1", snow_col = "#FFC0CB",
      ribbon = TRUE, ribbon_col = "#FFB6C1", ribbon_width = 1.2
    ),
    "pastel dream" = list(
      bg_col = "#1C1C22", tree_cols = c("#A7C7E7", "#F4C2C2", "#C1E1C1"),
      trunk_col = "#5B4636", decor_cols = c("#FFFFFF", "#FFD1DC"),
      star_col = "#FFF1A8", text_col = "#FDF6F0", snow_col = "#EDEDED",
      ribbon = TRUE, ribbon_col = "#F4C2C2", ribbon_width = 1.1
    ),
    "electric neon" = list(
      bg_col = "#05060A", tree_cols = c("#3B82F6", "#8B5CF6", "#22D3EE"),
      trunk_col = "#1F2937", decor_cols = c("#F9FAFB", "#E879F9"),
      star_col = "#FDE68A", text_col = "#E0E7FF", snow_col = "#E5E7EB",
      ribbon = TRUE, ribbon_col = "#8B5CF6", ribbon_width = 1.0
    ),
    "olive earth" = list(
      bg_col = "#1a1a1a", tree_cols = c("#556B2F", "#6B8E23", "#808000"),
      trunk_col = "#5D4037", decor_cols = c("#CD853F", "#F4C2C2"),
      star_col = "#FFC000", text_col = "#D8BFD8", snow_col = "white",
      ribbon = FALSE, ribbon_col = NA, ribbon_width = 0
    )
  )

  # ---- Input validation ----
  # Validate style parameter (can be integer index or character name)
  if (length(style) != 1 || is.na(style)) {
    stop("style must be a single value (integer index or character name).", call. = FALSE)
  }
  
  # Determine style key
  if (is.numeric(style)) {
    style_idx <- as.integer(style)
    if (style_idx < 1 || style_idx > length(styles)) {
      stop(sprintf("style index must be between 1 and %d.", length(styles)), call. = FALSE)
    }
    style_key <- names(styles)[style_idx]
  } else if (is.character(style)) {
    if (!(style %in% names(styles))) {
      stop(sprintf("style name '%s' not found. Available styles: %s.", style, paste(names(styles), collapse = ", ")), call. = FALSE)
    }
    style_key <- style
  } else {
    stop("style must be either an integer index or a character name.", call. = FALSE)
  }
  
  cfg <- styles[[style_key]]

  if (!is.character(text) || length(text) != 1 || is.na(text)) {
    stop("text must be a single character string.", call. = FALSE)
  }

  if (!is.numeric(text_x) || length(text_x) != 1 || is.na(text_x)) {
    stop("text_x must be numeric scalar.", call. = FALSE)
  }

  if (!is.numeric(text_y) || length(text_y) != 1 || is.na(text_y)) {
    stop("text_y must be numeric scalar.", call. = FALSE)
  }

  if (!is.numeric(text_size) || length(text_size) != 1 || is.na(text_size)) {
    stop("text_size must be numeric scalar.", call. = FALSE)
  }

  if (!is.numeric(text_lineheight) || length(text_lineheight) != 1 || is.na(text_lineheight)) {
    stop("text_lineheight must be numeric scalar.", call. = FALSE)
  }

  # Validate anno_text, anno_pos, and anno_size
  if (!is.null(anno_text)) {
    if (!is.character(anno_text) || length(anno_text) != 1 || is.na(anno_text)) {
      stop("anno_text must be NULL or a single character string.", call. = FALSE)
    }
    if (!is.character(anno_pos) || length(anno_pos) != 1 || is.na(anno_pos)) {
      stop("anno_pos must be a single character string.", call. = FALSE)
    }
    valid_positions <- c("tl", "tr", "bl", "br")
    if (!(anno_pos %in% valid_positions)) {
      stop(sprintf("anno_pos must be one of: %s.", paste(valid_positions, collapse = ", ")), call. = FALSE)
    }
  }
  if (!is.numeric(anno_size) || length(anno_size) != 1 || is.na(anno_size) || anno_size < 0) {
    stop("anno_size must be a positive numeric scalar.", call. = FALSE)
  }

  if (!is.numeric(seed) || length(seed) != 1 || is.na(seed)) {
    stop("seed must be numeric scalar.", call. = FALSE)
  }
  seed <- as.integer(seed)

  if (!is.numeric(n_frames) || length(n_frames) != 1 || is.na(n_frames) || n_frames < 2) {
    stop("n_frames must be an integer >= 2.", call. = FALSE)
  }
  n_frames <- as.integer(n_frames)

  if (!is.numeric(fps) || length(fps) != 1 || is.na(fps) || fps < 1) {
    stop("fps must be an integer >= 1.", call. = FALSE)
  }
  fps <- as.integer(fps)

  if (!is.numeric(width) || length(width) != 1 || is.na(width) || width < 50) {
    stop("width must be an integer >= 50.", call. = FALSE)
  }
  width <- as.integer(width)

  if (!is.numeric(height) || length(height) != 1 || is.na(height) || height < 50) {
    stop("height must be an integer >= 50.", call. = FALSE)
  }
  height <- as.integer(height)

  if (!is.null(out_gif) && (!is.character(out_gif) || length(out_gif) != 1 || is.na(out_gif))) {
    stop("out_gif must be NULL or a single file path string.", call. = FALSE)
  }

  if (!is.logical(loop) || length(loop) != 1 || is.na(loop)) {
    stop("loop must be TRUE/FALSE.", call. = FALSE)
  }

  if (!is.logical(show) || length(show) != 1 || is.na(show)) {
    stop("show must be TRUE/FALSE.", call. = FALSE)
  }

  # ---- Font setup (graceful fallback if Google font fetch fails) ----
  font_loaded <- FALSE
  
  # Try to load Christmas font (Great Vibes) from Google Fonts
  # Note: font_add_google is in sysfonts package, not showtext
  result <- tryCatch(
    {
      sysfonts::font_add_google("Great Vibes", "christmas_font")
      TRUE  # Success
    },
    error = function(e) {
      # Try fallback to system font if Google Fonts fails
      sys_font <- system.file("fonts", "GreatVibes-Regular.ttf", package = "showtext")
      if (file.exists(sys_font)) {
        tryCatch({
          sysfonts::font_add("christmas_font", regular = sys_font)
          TRUE  # Success with system font
        }, error = function(e2) {
          FALSE  # Both methods failed
        })
      } else {
        FALSE  # System font file doesn't exist
      }
    },
    warning = function(w) {
      # If there's a warning but no error, font might still be loaded
      # Try to verify by checking if we can proceed
      TRUE
    }
  )
  
  font_loaded <- isTRUE(result)
  
  # Enable showtext - must be enabled for fonts to work
  # Use showtext_auto() to enable for all new devices created by gganimate
  showtext::showtext_auto()
  
  # If font loading failed, warn user (but continue with default font)
  if (!font_loaded) {
    warning("Christmas font (Great Vibes) could not be loaded. Using default font.", call. = FALSE)
  }
  
  set.seed(seed)

  # ---- Helpers ----
  get_star_polygon <- function(x_center, y_center, radius) {
    angles <- seq(pi/2, 2.5 * pi, length.out = 11)[-11]
    radii <- rep(c(radius, radius * 0.4), 5)
    data.frame(
      x = x_center + radii * cos(angles),
      y = y_center + radii * sin(angles)
    )
  }

  generate_tree_data <- function(cfg) {
    n_leaves <- 800
    h <- runif(n_leaves, 0, 1)
    base_r <- (1 - h)
    layer_cycle <- (h * 7) %% 1
    r <- base_r * 0.65 * (0.4 + 0.6 * (1 - layer_cycle)^0.7)
    theta <- runif(n_leaves, 0, 2 * pi)

    df_tree <- data.frame(
      x = r * cos(theta),
      y = h - 0.5,
      z = r * sin(theta),
      col = sample(cfg$tree_cols, n_leaves, replace = TRUE),
      size = runif(n_leaves, 0.6, 1.8),
      type = "tree",
      alpha = 0.95
    )

    n_trunk <- 1000
    h_trunk <- runif(n_trunk, -0.7, -0.45)
    r_trunk <- 0.12
    theta_trunk <- runif(n_trunk, 0, 2 * pi)

    df_trunk <- data.frame(
      x = r_trunk * cos(theta_trunk),
      y = h_trunk,
      z = r_trunk * sin(theta_trunk),
      col = cfg$trunk_col,
      size = 1.2,
      type = "trunk",
      alpha = 1
    )

    n_decor <- 600
    h_dec <- runif(n_decor, 0, 0.95)
    base_r_dec <- (1 - h_dec)
    layer_cycle_dec <- (h_dec * 7) %% 1
    r_dec <- base_r_dec * 0.68 * (0.4 + 0.6 * (1 - layer_cycle_dec)^0.7)
    theta_dec <- runif(n_decor, 0, 2 * pi)

    df_decor <- data.frame(
      x = r_dec * cos(theta_dec),
      y = h_dec - 0.5,
      z = r_dec * sin(theta_dec),
      col = sample(cfg$decor_cols, n_decor, replace = TRUE),
      size = runif(n_decor, 2, 4),
      type = "decor",
      alpha = 1
    )

    df_ribbon <- NULL
    if (isTRUE(cfg$ribbon)) {
      n_rib <- 6000
      h_rib <- seq(0, 0.95, length.out = n_rib)
      base_r_rib <- (1 - h_rib) * 0.65 * 1.05
      theta_rib <- 10 * pi * h_rib

      df_ribbon <- data.frame(
        x = base_r_rib * cos(theta_rib),
        y = h_rib - 0.5,
        z = base_r_rib * sin(theta_rib),
        col = cfg$ribbon_col,
        size = cfg$ribbon_width,
        type = "ribbon",
        alpha = 1
      )
    }

    dplyr::bind_rows(df_trunk, df_tree, df_decor, df_ribbon)
  }

  generate_snow <- function(cfg, n_flakes = 250) {
    data.frame(
      x = runif(n_flakes, -1, 1),
      y = runif(n_flakes, -0.8, 1.2),
      z = runif(n_flakes, -1, 1),
      col = cfg$snow_col,
      size = runif(n_flakes, 0.5, 2),
      type = "snow",
      alpha = runif(n_flakes, 0.5, 0.9),
      speed = runif(n_flakes, 0.015, 0.035)
    )
  }

  process_frame <- function(frame_id, static_data, snow_data, n_frames) {
    angle <- 2 * pi * (frame_id / n_frames)

    tree_rot <- dplyr::mutate(
      static_data,
      x_rot = x * cos(angle) - z * sin(angle),
      z_rot = z * cos(angle) + x * sin(angle),
      y_final = y
    )

    snow_curr <- dplyr::mutate(
      snow_data,
      y_final = -0.8 + (y - frame_id * speed * (-0.85)) %% 2,
      x_rot = x,
      z_rot = z
    )

    dplyr::bind_rows(tree_rot, snow_curr) %>%
      dplyr::mutate(
        depth = 1 / (2.5 - z_rot),
        x_proj = x_rot * depth * 2,
        y_proj = y_final * depth * 2,
        size_vis = size * depth * 1.5,
        alpha_vis = alpha * ifelse(type == "snow", 1, (z_rot + 1.2) / 2),
        frame = frame_id
      ) %>%
      dplyr::arrange(depth)
  }

  # ---- Data ----
  static_data <- generate_tree_data(cfg)
  snow_data <- generate_snow(cfg, n_flakes = 250)
  star_shape <- get_star_polygon(0, 0.4, 0.03)

  all_frames <- lapply(
    seq_len(n_frames),
    process_frame,
    static_data = static_data,
    snow_data = snow_data,
    n_frames = n_frames
  ) %>%
    dplyr::bind_rows()

  # ---- Plot ----
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = all_frames,
      ggplot2::aes(
        x = x_proj,
        y = y_proj,
        color = I(col),
        size = I(size_vis),
        alpha = I(alpha_vis)
      ),
      shape = 19
    ) +
    ggplot2::geom_polygon(
      data = star_shape,
      ggplot2::aes(x = x, y = y),
      fill = cfg$star_col,
      color = "white",
      linewidth = 0.3
    ) +
    ggplot2::annotate(
      "text",
      x = text_x,
      y = text_y,
      label = text,  # user can pass "...\n..."
      family = if (font_loaded) "christmas_font" else "sans",
      color = cfg$text_col,
      size = text_size,
      lineheight = text_lineheight,
      hjust = 0.5,
      vjust = 0.5
    ) +
    {
      # Add corner annotation if specified
      if (!is.null(anno_text)) {
        # Determine corner coordinates and justification (caption style - slightly inset)
        corner_coords <- switch(
          anno_pos,
          "tl" = list(x = -0.90, y = 0.9, hjust = 0, vjust = 1),
          "tr" = list(x = 0.90, y = 0.9, hjust = 1, vjust = 1),
          "bl" = list(x = -0.90, y = -0.8, hjust = 0, vjust = 0),
          "br" = list(x = 0.90, y = -0.8, hjust = 1, vjust = 0)
        )
        
        ggplot2::annotate(
          "text",
          x = corner_coords$x,
          y = corner_coords$y,
          label = anno_text,
          color = cfg$text_col,
          size = anno_size,
          lineheight = 1.0,
          hjust = corner_coords$hjust,
          vjust = corner_coords$vjust
        )
      } else {
        NULL
      }
    } +
    ggplot2::scale_size_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_fixed(xlim = c(-0.85, 0.85), ylim = c(-0.8, 0.9)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = grid::unit(c(0.05, 0.05, 0.05, 0.05), "npc"),
      plot.background = ggplot2::element_rect(fill = cfg$bg_col, color = NA),
      panel.background = ggplot2::element_rect(fill = cfg$bg_col, color = NA)
    ) +
    gganimate::transition_manual(frame)

  # ---- Render ----
  # showtext_auto() is already enabled above, which will automatically
  # enable showtext for any new graphics devices created by gganimate
  # No need to call showtext_begin() here as it requires an active device
  anim <- gganimate::animate(
    p,
    nframes = n_frames,
    fps = fps,
    width = width,
    height = height,
    bg = cfg$bg_col,
    renderer = gganimate::gifski_renderer(loop = loop)
  )

  if (isTRUE(show)) {
    print(anim)
  }

  if (!is.null(out_gif)) {
    gganimate::anim_save(out_gif, animation = anim)
  }

  invisible(list(plot = p, animation = anim, cfg = cfg))
}




#' Create a panel GIF from multiple style GIFs
#'
#' @param styles Vector of style indices or names (default 1:9). Can be integer indices (1-10) or
#'   character style names (e.g., `c("classic christmas", "warm cozy", "vibrant primary")`).
#'   See `draw_xmas_tree_gif` for available style names.
#'   The length of `styles` must be divisible by `ncol`.
#' @param out_gif Output GIF file path.
#' @param text_fun Function taking style (integer or character) and returning label text for that panel.
#' @param ncol Number of columns (default 3). Number of rows is calculated as `length(styles) / ncol`.
#' @param n_frames Frames per individual GIF (must match across).
#' @param fps Frames per second (must match across).
#' @param width,height Pixel size per tile (each individual GIF).
#' @param text_size Numeric text size for the main title text (default: 12).
#' @param anno_size Numeric text size for the annotation text (default: 4).
#' @param loop Logical; loop the final GIF.
#' @param tmp_dir Where to write temporary per-style GIFs.
#' @param show Logical; show final GIF in interactive sessions.
#'
#' @return Invisibly returns the `magick-image` animation object.
#' @export
draw_xmas_tree_panel_gif <- function(
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
) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required but not installed.", call. = FALSE)
  }
  if (!requireNamespace("fs", quietly = TRUE)) {
    stop("Package 'fs' is required but not installed.", call. = FALSE)
  }

  if (!is.numeric(ncol) || length(ncol) != 1 || is.na(ncol) ||
      ncol < 1 || ncol != as.integer(ncol)) {
    stop("ncol must be a positive integer.", call. = FALSE)
  }
  ncol <- as.integer(ncol)

  # Validate styles parameter
  n_tiles <- length(styles)
  if (n_tiles == 0) {
    stop("styles must have at least one element.", call. = FALSE)
  }
  
  # Check if styles is numeric or character (not mixed)
  is_numeric <- is.numeric(styles)
  is_character <- is.character(styles)
  
  if (!is_numeric && !is_character) {
    stop("styles must be either a numeric vector (indices) or character vector (style names).", call. = FALSE)
  }
  
  # Valid style names (same as in draw_xmas_tree_gif)
  valid_style_names <- c(
    "classic christmas", "warm cozy", "vibrant primary", "winter frost", "icy night",
    "monochrome", "pink romance", "pastel dream", "electric neon", "olive earth"
  )
  
  # Validate based on type
  if (is_numeric) {
    # Validate numeric indices are in valid range
    styles_int <- as.integer(styles)
    if (any(is.na(styles_int)) || any(styles_int < 1) || any(styles_int > length(valid_style_names))) {
      stop(sprintf("When styles is numeric, all values must be integers between 1 and %d.", 
                   length(valid_style_names)), call. = FALSE)
    }
  } else if (is_character) {
    # Validate character names are valid style names
    invalid_names <- styles[!(styles %in% valid_style_names)]
    if (length(invalid_names) > 0) {
      stop(sprintf("Invalid style name(s): %s. Valid style names: %s.",
                   paste(invalid_names, collapse = ", "),
                   paste(valid_style_names, collapse = ", ")), call. = FALSE)
    }
  }
  
  # Check panel dimensions
  nrow <- n_tiles / ncol
  if (nrow != as.integer(nrow)) {
    stop(sprintf("Cannot create panel: length(styles) = %d is not divisible by ncol = %d.",
                 n_tiles, ncol), call. = FALSE)
  }
  nrow <- as.integer(nrow)

  # Check for unique values
  if (length(unique(styles)) != n_tiles) {
    stop("styles must contain unique values.", call. = FALSE)
  }
  
  if (!is.function(text_fun)) {
    stop("text_fun must be a function(style) -> character(1).", call. = FALSE)
  }

  # Set ImageMagick resource limits to prevent cache exhaustion
  tryCatch({
    magick::image_resource_limit("memory", "8GiB")
    magick::image_resource_limit("map", "16GiB")
    magick::image_resource_limit("disk", "16GiB")
  }, error = function(e) {
    # Resource limits may not be available in all ImageMagick versions
    warning("Could not set ImageMagick resource limits.", call. = FALSE)
  })
  
  if (is.null(tmp_dir)) tmp_dir <- fs::path_temp("xmas_panel_tiles")
  fs::dir_create(tmp_dir)

  # Generate tile paths based on style type
  tile_names <- if (is_numeric) {
    sprintf("tile_%02d.gif", as.integer(styles))
  } else {
    sanitized <- gsub("[^a-zA-Z0-9]", "_", styles)
    sprintf("tile_%s.gif", sanitized)
  }
  tile_paths <- fs::path(tmp_dir, tile_names)

  # Helper to get style name from style value
  # Since we've already validated styles, we can safely convert
  get_style_name <- function(style_val) {
    if (is_numeric) {
      # Convert numeric index to style name
      style_idx <- as.integer(style_val)
      return(valid_style_names[style_idx])
    } else {
      # Character style name - return as is
      return(style_val)
    }
  }

  # 1) Generate tile GIFs
  for (k in seq_along(styles)) {
    style_val <- styles[k]
    style_name <- get_style_name(style_val)
    draw_xmas_tree_gif(
      style = style_val,
      text = "Merry Christmas",
      anno_text = style_name,
      anno_pos = "br",
      text_size = text_size,
      anno_size = anno_size,
      n_frames = n_frames,
      fps = fps,
      width = width,
      height = height,
      out_gif = tile_paths[k],
      loop = TRUE,
      show = FALSE
    )
  }

  # 2) Read all GIFs and validate frame counts
  # Read GIFs one at a time to check frame counts without loading all into memory
  frame_counts <- integer(length(tile_paths))
  for (i in seq_along(tile_paths)) {
    gif_temp <- magick::image_read(tile_paths[i])
    frame_counts[i] <- length(gif_temp)
    magick::image_destroy(gif_temp)
    rm(gif_temp)
    if (i %% 3 == 0) gc()  # Garbage collect periodically
  }
  n_min <- min(frame_counts)
  if (any(frame_counts != n_min)) {
    warning(
      sprintf("Frame counts differ (%s). Trimming all to %d frames.",
              paste(frame_counts, collapse = ", "), n_min),
      call. = FALSE
    )
  }

  # 3) Process and combine frames incrementally to reduce memory usage
  # Write frames to temporary files, then combine at the end
  frame_dir <- fs::path(tmp_dir, "panel_frames")
  fs::dir_create(frame_dir)
  on.exit({
    if (fs::dir_exists(frame_dir)) fs::dir_delete(frame_dir)
  }, add = TRUE)
  
  frame_files <- character(n_min)
  
  for (f in seq_len(n_min)) {
    tryCatch({
      # Read frame f from each GIF (one at a time to save memory)
      imgs_f <- vector("list", n_tiles)
      for (i in seq_along(tile_paths)) {
        gif_temp <- magick::image_read(tile_paths[i])
        if (length(gif_temp) >= f) {
          frame_img <- gif_temp[f]
          # Ensure it's a single image
          if (length(frame_img) > 1) frame_img <- frame_img[1]
          imgs_f[[i]] <- frame_img
        }
        magick::image_destroy(gif_temp)
        rm(gif_temp)
        if (i %% 3 == 0) gc()  # Garbage collect periodically
      }

      # Split into rows
      rows <- split(imgs_f, rep(seq_len(nrow), each = ncol))

      # Build each row (horizontal append)
      row_imgs <- lapply(rows, function(row_list) {
        # Convert list to vector for image_append
        row_vec <- do.call(c, row_list)
        magick::image_append(row_vec, stack = FALSE)  # horizontal
      })

      # Stack rows vertically
      rows_vec <- do.call(c, row_imgs)
      panel_frame <- magick::image_append(rows_vec, stack = TRUE)
      
      # Save frame to temporary file (use lower quality to reduce memory)
      frame_file <- fs::path(frame_dir, sprintf("frame_%04d.png", f))
      magick::image_write(panel_frame, path = frame_file, format = "png", quality = 85)
      frame_files[f] <- frame_file
      
      # Clean up intermediate objects
      rm(imgs_f, rows, row_imgs, rows_vec, panel_frame)
      if (f %% 10 == 0) gc()  # Garbage collect every 10 frames
    }, error = function(e) {
      stop(sprintf("Error processing frame %d: %s", f, conditionMessage(e)), call. = FALSE)
    })
  }

  # 4) Read frame files and combine into GIF in smaller batches
  # Use smaller batches to reduce memory pressure
  batch_size <- min(20, max(5, floor(n_min / 10)))  # Adaptive batch size
  n_batches <- ceiling(n_min / batch_size)
  panel_frames_list <- vector("list", n_batches)
  
  for (batch_idx in seq_len(n_batches)) {
    start_idx <- (batch_idx - 1) * batch_size + 1
    end_idx <- min(batch_idx * batch_size, n_min)
    batch_files <- frame_files[start_idx:end_idx]
    
    # Read batch frames
    batch_frames <- lapply(batch_files, function(f) {
      img <- magick::image_read(f)
      # Ensure single frame
      if (length(img) > 1) img <- img[1]
      img
    })
    panel_frames_list[[batch_idx]] <- do.call(c, batch_frames)
    
    # Clean up immediately
    rm(batch_frames)
    if (batch_idx %% 2 == 0) gc()  # Garbage collect every other batch
  }
  
  # Combine batches incrementally to avoid holding everything at once
  if (n_batches > 1) {
    # Start with first batch
    panel_vec <- panel_frames_list[[1]]
    
    # Combine remaining batches one at a time
    for (batch_idx in 2:n_batches) {
      next_batch <- panel_frames_list[[batch_idx]]
      panel_vec <- c(panel_vec, next_batch)
      rm(next_batch)
      if (batch_idx %% 3 == 0) gc()  # Garbage collect periodically
    }
    
    # Clean up list
    rm(panel_frames_list)
    gc()
  } else {
    panel_vec <- panel_frames_list[[1]]
    rm(panel_frames_list)
    gc()
  }
  
  # Create animation with optimization
  panel_gif <- tryCatch({
    magick::image_animate(
      panel_vec, 
      fps = fps, 
      loop = if (isTRUE(loop)) 0 else 1,
      optimize = TRUE,
      dispose = "previous"
    )
  }, error = function(e) {
    # Fallback: try without optimize if it fails
    warning("Animation optimization failed, trying without optimize option.", call. = FALSE)
    magick::image_animate(
      panel_vec, 
      fps = fps, 
      loop = if (isTRUE(loop)) 0 else 1
    )
  })
  
  # Clean up before writing
  rm(panel_vec)
  gc()

  # Write with compression and memory-efficient options
  # Use lower quality if needed to reduce memory
  tryCatch({
    magick::image_write(
      panel_gif, 
      path = out_gif,
      quality = 85
    )
  }, error = function(e) {
    # If still fails, try with even lower quality
    warning("First write attempt failed, retrying with lower quality.", call. = FALSE)
    magick::image_write(
      panel_gif, 
      path = out_gif,
      quality = 75
    )
  })
  
  # Show if requested (before cleanup)
  if (isTRUE(show)) print(panel_gif)
  
  # Note: panel_gif is returned invisibly, so we don't remove it here
  invisible(panel_gif)
}
