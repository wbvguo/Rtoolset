#' Draw a 2D heart shape image
#'
#' Creates a 2D heart shape image using ggplot2. The heart is generated using
#' parametric equations and can be saved to a PNG file. The saved image can
#' then be used with `draw_heart_step()` to create a LEGO mosaic.
#'
#' @param out_png Character scalar. Path where the heart image will be saved.
#'   If NULL, the image is not saved but the ggplot object is returned.
#' @param fill Character. Fill color for the heart (default: "red").
#' @param color Character. Border color for the heart (default: "firebrick").
#' @param alpha Numeric. Transparency level (0-1, default: 0.9).
#' @param show Logical. If TRUE, displays the heart plot. Defaults to `interactive()`.
#' @param width,height Numeric. Width and height of the saved image in inches
#'   (default: 4). Only used when `out_png` is provided.
#'
#' @return A `ggplot` object. If `out_png` is provided, the image is also saved to disk.
#'
#' @examples
#' \dontrun{
#' # Generate and display a heart shape
#' heart <- draw_heart()
#' print(heart)
#'
#' # Generate and save to file
#' heart <- draw_heart(out_png = "myheart.png")
#'
#' # Customize colors
#' heart <- draw_heart(out_png = "pink_heart.png", fill = "pink", color = "hotpink")
#' }
#'
#' @export
draw_heart <- function(
  out_png = NULL,
  fill = "red",
  color = "firebrick",
  alpha = 0.9,
  show = interactive(),
  width = 4,
  height = 4
) {
  # ---- Package dependency checks ----
  .require_pkg <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required but not installed.", pkg), call. = FALSE)
    }
  }
  
  # Check for required packages
  pkgs <- c("ggplot2")
  invisible(lapply(pkgs, .require_pkg))
  
  # ---- Input validation ----
  if (!is.null(out_png)) {
    if (!is.character(out_png) || length(out_png) != 1 || is.na(out_png)) {
      stop("out_png must be NULL or a single character string.", call. = FALSE)
    }
  }
  
  if (!is.character(fill) || length(fill) != 1 || is.na(fill)) {
    stop("fill must be a single character string.", call. = FALSE)
  }
  
  if (!is.character(color) || length(color) != 1 || is.na(color)) {
    stop("color must be a single character string.", call. = FALSE)
  }
  
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha < 0 || alpha > 1) {
    stop("alpha must be a numeric value between 0 and 1.", call. = FALSE)
  }
  
  if (!is.logical(show) || length(show) != 1 || is.na(show)) {
    stop("show must be TRUE/FALSE.", call. = FALSE)
  }
  
  if (!is.numeric(width) || length(width) != 1 || is.na(width) || width <= 0) {
    stop("width must be a positive numeric value.", call. = FALSE)
  }
  
  if (!is.numeric(height) || length(height) != 1 || is.na(height) || height <= 0) {
    stop("height must be a positive numeric value.", call. = FALSE)
  }
  
  # ---- Generate heart shape ----
  dat <- data.frame(t = seq(0, 2*pi, by = 0.01))
  x <- function(t) 16 * sin(t)^3
  y <- function(t) 13*cos(t) - 5*cos(2*t) - 2*cos(3*t) - cos(4*t)
  dat$y <- y(dat$t)
  dat$x <- x(dat$t)
  
  heart <- ggplot2::ggplot(dat, ggplot2::aes(x, y)) +  
    ggplot2::geom_polygon(fill = fill, col = color, alpha = alpha) +  
    ggplot2::coord_fixed() +
    ggplot2::theme_void()
  
  # ---- Save to file if requested ----
  if (!is.null(out_png)) {
    # Create directory if it doesn't exist
    dir_path <- dirname(out_png)
    if (dir_path != "." && !dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
    ggplot2::ggsave(heart, filename = out_png, width = width, height = height, bg = "white")
  }
  
  # ---- Display plot if requested ----
  if (isTRUE(show)) {
    print(heart)
  }
  
  return(heart)
}

#' Convert heart image to LEGO mosaic and extract build steps
#'
#' Converts a heart image to a LEGO mosaic using \pkg{brickr} and extracts
#' the build steps/panels. The output can be used with `draw_heart_gif()` to
#' create an animated GIF.
#'
#' @param image_path Character scalar. Path to the PNG image file to convert.
#'   If NULL and `heart_image` is also NULL, a simple heart shape will be generated.
#' @param heart_image Optional. Can be a ggplot object (output from `draw_heart()`)
#'   or a PNG image array. If provided, `image_path` is ignored.
#' @param num_steps Integer. Number of build steps for the instructions (default: 6).
#' @param width,height Integer. Dimensions for the LEGO mosaic (default: 50x50).
#' @param show Logical. If TRUE, displays the final mosaic plot. Defaults to `FALSE` 
#'   (display is currently disabled due to ggplot2/brickr compatibility issues).
#'
#' @return A list with elements:
#' \describe{
#'   \item{mosaic}{The `brickr` mosaic object.}
#'   \item{build}{The build instructions object.}
#'   \item{steps}{List of ggplot objects for each build step.}
#' }
#'
#' @examples
#' \dontrun{
#' # Using an existing image file
#' heart_steps <- draw_heart_step(image_path = "heart.png", num_steps = 6)
#'
#' # Using a ggplot object from draw_heart()
#' heart_plot <- draw_heart()
#' heart_steps <- draw_heart_step(heart_image = heart_plot, num_steps = 6)
#'
#' # Display the mosaic
#' print(brickr::build_mosaic(heart_steps$mosaic))
#' }
#'
#' @export
draw_heart_step <- function(
  image_path = NULL,
  heart_image = NULL,
  num_steps = 6,
  width = 50,
  height = 50,
  show = FALSE
) {
  # ---- Package dependency checks ----
  .require_pkg <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required but not installed.", pkg), call. = FALSE)
    }
  }
  
  # Check for required packages
  pkgs <- c("brickr", "png", "dplyr", "ggplot2")
  invisible(lapply(pkgs, .require_pkg))
  
  # Load brickr to make internal data (lego_colors) available
  # brickr functions require access to internal datasets that aren't accessible via ::
  if (!("package:brickr" %in% search())) {
    suppressPackageStartupMessages(library(brickr, character.only = TRUE))
  }
  
  # ---- Input validation ----
  if (!is.null(image_path) && !is.null(heart_image)) {
    warning("Both image_path and heart_image provided. Using heart_image and ignoring image_path.", call. = FALSE)
    image_path <- NULL
  }
  
  if (!is.null(image_path)) {
    if (!is.character(image_path) || length(image_path) != 1 || is.na(image_path)) {
      stop("image_path must be NULL or a single character string.", call. = FALSE)
    }
    if (!file.exists(image_path)) {
      stop(sprintf("Image file not found: %s", image_path), call. = FALSE)
    }
  }
  
  # Check if heart_image is a ggplot object
  is_ggplot <- !is.null(heart_image) && inherits(heart_image, "ggplot")
  is_png_array <- !is.null(heart_image) && is.array(heart_image) && length(dim(heart_image)) == 3
  
  if (!is.null(heart_image) && !is_ggplot && !is_png_array) {
    stop("heart_image must be a ggplot object or a 3D array (PNG image array).", call. = FALSE)
  }
  
  if (is.null(image_path) && is.null(heart_image)) {
    # Generate heart programmatically using draw_heart()
    heart_image <- draw_heart(show = FALSE)
    is_ggplot <- TRUE
    is_png_array <- FALSE
  }
  
  if (!is.numeric(num_steps) || length(num_steps) != 1 || is.na(num_steps) || num_steps < 1) {
    stop("num_steps must be a positive integer.", call. = FALSE)
  }
  num_steps <- as.integer(num_steps)
  
  if (!is.numeric(width) || length(width) != 1 || is.na(width) || width < 1) {
    stop("width must be a positive integer.", call. = FALSE)
  }
  width <- as.integer(width)
  
  if (!is.numeric(height) || length(height) != 1 || is.na(height) || height < 1) {
    stop("height must be a positive integer.", call. = FALSE)
  }
  height <- as.integer(height)
  
  if (!is.logical(show) || length(show) != 1 || is.na(show)) {
    stop("show must be TRUE/FALSE.", call. = FALSE)
  }
  
  # ---- Load or convert image ----
  if (!is.null(image_path)) {
    heart_png <- png::readPNG(image_path)
  } else if (is_ggplot) {
    # Convert ggplot to PNG array via temporary file
    temp_file <- tempfile(fileext = ".png")
    # Convert ggplot to PNG with white background
    ggplot2::ggsave(heart_image, filename = temp_file, width = 4, height = 4, bg = "white")
    on.exit({
      if (file.exists(temp_file)) unlink(temp_file)
    }, add = TRUE)
    heart_png <- png::readPNG(temp_file)
  } else {
    # heart_image is already a PNG array
    heart_png <- heart_image
  }
  
  # ---- Convert to mosaic ----
  img_mosaic <- brickr::image_to_mosaic(heart_png)
  
  # ---- Build instructions ----
  build <- brickr::build_instructions(img_mosaic, num_steps = num_steps)
  
  # ---- Extract subplots for each step ----
  # Optimized: use lapply instead of for loop
  step_labels <- sprintf("Step %02d", seq_len(num_steps))
  steps <- lapply(step_labels, function(step_label) {
    # Create a copy of build and filter its data
    step_plot <- build
    step_plot$data <- step_plot$data %>% 
      dplyr::filter(Step == step_label)
    # Add scales to the plot
    step_plot + 
      ggplot2::scale_x_continuous(limits = c(0, width)) + 
      ggplot2::scale_y_continuous(limits = c(0, height))
  })
  
  # ---- Display mosaic if requested ----
  if (isTRUE(show)) {
    tryCatch({
      print(brickr::build_mosaic(img_mosaic))
    }, error = function(e) {
      warning("Could not display mosaic plot. This may be due to a ggplot2/brickr compatibility issue. ",
              "The mosaic was created successfully and can be used for building steps. ",
              "Error: ", conditionMessage(e), call. = FALSE)
    })
  }
  
  invisible(list(mosaic = img_mosaic, build = build, steps = steps))
}

#' Create a LEGO-style animated heart GIF
#'
#' Creates an animated GIF showing the step-by-step build process of a LEGO heart
#' mosaic from the build steps. The steps can be generated using `draw_heart_step()`.
#'
#' @param heart_steps Optional list. Output from `draw_heart_step()` containing
#'   the build steps. If provided, `image_path`, `heart_image`, `num_steps`,
#'   `width`, and `height` are ignored.
#' @param image_path Character scalar. Path to the PNG image file to convert.
#'   If NULL and `heart_image` is also NULL, a simple heart shape will be generated.
#'   Ignored if `heart_steps` is provided.
#' @param heart_image Optional. Can be a ggplot object (output from `draw_heart()`)
#'   or a PNG image array. If provided, `image_path` is ignored.
#'   Ignored if `heart_steps` is provided.
#' @param num_steps Integer. Number of build steps in the animation (default: 6).
#'   Ignored if `heart_steps` is provided.
#' @param out_gif Character path. Output path for the GIF file.
#'   Default: "myheart.gif" in the current working directory.
#' @param width,height Integer. Dimensions for the LEGO mosaic (default: 50x50).
#'   Ignored if `heart_steps` is provided.
#' @param interval Numeric. Delay between frames in seconds (default: 0.5).
#'   Larger values create slower animations. Ignored if `heart_steps` is provided.
#' @param show Logical. If TRUE, displays the GIF after saving. Defaults to `interactive()`.
#'
#' @return Invisibly returns the `heart_steps` list (same structure as `draw_heart_step()`).
#'
#' @examples
#' \dontrun{
#' # Create steps and GIF in one step
#' draw_heart_gif(image_path = "heart.png", out_gif = "heart_animation.gif")
#'
#' # Or create steps first, then GIF
#' heart_steps <- draw_heart_step(image_path = "heart.png", num_steps = 6)
#' draw_heart_gif(heart_steps = heart_steps, out_gif = "heart_animation.gif")
#'
#' # Control animation speed (slower with larger interval)
#' draw_heart_gif(heart_steps = heart_steps, out_gif = "slow_heart.gif", interval = 1.0)
#'
#' # Display the GIF after saving
#' draw_heart_gif(heart_steps = heart_steps, out_gif = "myheart.gif", show = TRUE)
#'
#' # Full workflow
#' heart_plot <- draw_heart(out_png = "myheart.png")
#' heart_steps <- draw_heart_step(heart_image = heart_plot, num_steps = 6)
#' draw_heart_gif(heart_steps = heart_steps, out_gif = "myheart.gif", interval = 0.5, show = TRUE)
#' }
#'
#' @export
draw_heart_gif <- function(
  heart_steps = NULL,
  image_path = NULL,
  heart_image = NULL,
  num_steps = 6,
  out_gif = "myheart.gif",
  width = 50,
  height = 50,
  interval = 0.5,
  show = interactive()
) {
  # ---- Package dependency checks ----
  .require_pkg <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required but not installed.", pkg), call. = FALSE)
    }
  }
  
  # Check for required packages
  pkgs <- c("animation")
  invisible(lapply(pkgs, .require_pkg))
  
  # ---- Input validation ----
  if (!is.null(heart_steps)) {
    # Validate heart_steps structure
    if (!is.list(heart_steps) || 
        !all(c("mosaic", "build", "steps") %in% names(heart_steps))) {
      stop("heart_steps must be a list with elements 'mosaic', 'build', and 'steps' (output from draw_heart_step()).", call. = FALSE)
    }
    steps <- heart_steps$steps
  } else {
    # Create heart steps if not provided
    heart_steps <- draw_heart_step(
      image_path = image_path,
      heart_image = heart_image,
      num_steps = num_steps,
      width = width,
      height = height,
      show = FALSE
    )
    steps <- heart_steps$steps
  }
  
  if (!is.character(out_gif) || length(out_gif) != 1 || is.na(out_gif)) {
    stop("out_gif must be a single file path string.", call. = FALSE)
  }
  
  if (!is.numeric(interval) || length(interval) != 1 || is.na(interval) || interval <= 0) {
    stop("interval must be a positive numeric value (seconds between frames).", call. = FALSE)
  }
  
  if (!is.logical(show) || length(show) != 1 || is.na(show)) {
    stop("show must be TRUE/FALSE.", call. = FALSE)
  }
  
  # ---- Save GIF ----
  # Create directory if it doesn't exist
  dir_path <- dirname(out_gif)
  if (dir_path != "." && !dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Suppress graphics window and browser opening
  # Use PNG device instead of window device to avoid pop-up windows
  old_opts <- animation::ani.options(
    autobrowse = FALSE,
    interval = interval,
    ani.dev = "png",
    ani.type = "png"
  )
  on.exit(animation::ani.options(old_opts), add = TRUE)
  
  # Save GIF without opening windows
  animation::saveGIF(
    expr = {
      for (step_plot in steps) {
        plot(step_plot)
      }
    },
    movie.name = out_gif
  )
  
  # Display GIF if requested
  if (isTRUE(show)) {
    if (requireNamespace("magick", quietly = TRUE)) {
      # Ensure we use the correct path (handle relative paths)
      gif_path <- if (file.exists(out_gif)) {
        normalizePath(out_gif)
      } else {
        out_gif
      }
      gif_img <- magick::image_read(gif_path)
      print(gif_img)
    } else {
      warning("Package 'magick' is required to display the GIF. ",
              "Install it with: install.packages('magick')", call. = FALSE)
    }
  }
  
  invisible(heart_steps)
}
