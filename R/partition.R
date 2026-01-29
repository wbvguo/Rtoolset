#' Core optimizer: assign samples to groups to balance mean + SD (Gini loss)
#'
#' @description
#' Internal/core function that performs the random search and returns the best
#' grouping. No plotting and no file writing.
#'
#' The objective minimized is:
#' \deqn{loss = Gini(group\_means) + lambda * Gini(group\_sds)}
#'
#' @param score Numeric vector (length n). The scalar to balance.
#' @param group_sizes Integer vector of target group sizes. Must sum to \code{length(score)}.
#' @param lambda Numeric >= 0. Weight on SD-balance term. Default is \code{1}.
#' @param B Integer >= 1. Number of candidate partitions to evaluate.
#' @param method Character. One of \code{"blocked_permute"} or \code{"random_assign"}.
#' @param seed Optional integer seed.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{groups}: integer vector (length n) of group labels 1..K
#'   \item \code{loss}: best achieved loss
#'   \item \code{group_sizes}: integer vector of target sizes used
#' }
#'
#' @importFrom stats aggregate
#' @export
balance_partition_core <- function(
  score,
  group_sizes,
  lambda = 1,
  B = 50000,
  method = c("blocked_permute", "random_assign"),
  seed = NULL
) {
  method <- match.arg(method)

  if (!requireNamespace("DescTools", quietly = TRUE)) {
    stop("Package 'DescTools' is required. Please install it.")
  }

  if (!is.numeric(score)) stop("`score` must be numeric.")
  if (anyNA(score)) stop("`score` contains NA; handle missing values first.")
  n <- length(score)

  if (!is.numeric(group_sizes) || anyNA(group_sizes)) stop("`group_sizes` must be numeric/integer with no NA.")
  group_sizes <- as.integer(group_sizes)
  if (length(group_sizes) < 2) stop("`group_sizes` must have length >= 2.")
  if (any(group_sizes <= 0)) stop("All `group_sizes` must be positive.")
  if (sum(group_sizes) != n) stop("Sum of `group_sizes` must equal length(score).")

  if (!is.numeric(lambda) || length(lambda) != 1 || is.na(lambda) || lambda < 0) {
    stop("`lambda` must be a single numeric value >= 0.")
  }
  if (!is.numeric(B) || length(B) != 1 || is.na(B) || B < 1) stop("`B` must be >= 1.")
  B <- as.integer(B)

  if (!is.null(seed)) set.seed(seed)

  K <- length(group_sizes)
  ord <- order(score)

  compute_loss <- function(groups) {
    mu  <- tapply(score, groups, mean)
    sdv <- tapply(score, groups, stats::sd)
    if (anyNA(sdv)) return(Inf)
    DescTools::Gini(mu) + lambda * DescTools::Gini(sdv)
  }

  best_loss   <- Inf
  best_groups <- integer(n)

  for (b in seq_len(B)) {
    groups <- integer(n)

    if (method == "random_assign") {
      perm <- sample.int(n)
      pos  <- 1
      for (k in seq_len(K)) {
        idx <- perm[pos:(pos + group_sizes[k] - 1)]
        groups[idx] <- k
        pos <- pos + group_sizes[k]
      }
    } else {
      remaining <- group_sizes
      pos <- 1
      while (sum(remaining) > 0) {
        active <- which(remaining > 0)
        take   <- length(active)
        block  <- ord[pos:(pos + take - 1)]
        groups[block] <- sample(active)
        remaining[active] <- remaining[active] - 1
        pos <- pos + take
      }
    }

    loss <- compute_loss(groups)
    if (loss < best_loss) {
      best_loss   <- loss
      best_groups <- groups
    }
  }

  list(groups = best_groups, loss = best_loss, group_sizes = group_sizes)
}

#' Balanced partition of samples into groups using one numeric column (mean + SD)
#'
#' @description
#' User-facing wrapper around \code{\link{balance_partition_core}}.
#' Computes the best grouping using \code{score_col}, and optionally generates
#' a visualization and/or writes outputs to files.
#'
#' @param data A \code{data.frame} with samples in rows.
#' @param score_col Name of the numeric column to balance.
#' @param K Integer. Number of groups. Ignored if \code{group_sizes} is provided.
#' @param group_sizes Optional integer vector of target group sizes; must sum to \code{nrow(data)}.
#' @param id_col Optional sample ID column name. If \code{NULL}, uses row index.
#' @param lambda Numeric >= 0. Weight on SD-balance term. Default \code{1}.
#' @param B Integer >= 1. Number of random tries. Default \code{50000}.
#' @param method \code{"blocked_permute"} (default) or \code{"random_assign"}.
#' @param allow_unequal Logical. If TRUE and \code{n} is not divisible by \code{K},
#'   group sizes differ by at most 1. Ignored if \code{group_sizes} is provided.
#' @param seed Optional integer seed for reproducibility.
#'
#' @param output_dir Optional directory path. If provided, outputs are written here.
#' @param file_prefix Character. Prefix for output filenames.
#' @param output_csv Logical. If TRUE and \code{output_dir} is provided, write CSV
#'   of original data plus appended \code{.__group} and \code{.__score}.
#' @param output_plot Logical. If TRUE, generate a ggplot object and (if
#'   \code{output_dir} is provided) write a PNG.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{assignment}: data.frame with \code{sample_id}, \code{group}, \code{score}.
#'   \item \code{group_stats}: data.frame with per-group \code{n}, \code{mean}, \code{sd}.
#'   \item \code{loss}: best achieved loss.
#'   \item \code{group_sizes}: integer vector of target group sizes used.
#'   \item \code{plot}: ggplot object if \code{output_plot = TRUE}, else NULL.
#'   \item \code{files}: named list of written file paths if any were written, else NULL.
#'   \item \code{params}: list of parameters used.
#' }
#'
#' @importFrom stats aggregate
#' @importFrom dplyr .data
#' @importFrom ggplot2 after_stat
#' @export
balanced_partition <- function(
  data,
  score_col,
  K = NULL,
  group_sizes = NULL,
  id_col = NULL,
  lambda = 1,
  B = 50000,
  method = c("blocked_permute", "random_assign"),
  allow_unequal = TRUE,
  seed = NULL,
  output_dir = NULL,
  file_prefix = "balanced_partition",
  output_csv = TRUE,
  output_plot = FALSE
) {
  method <- match.arg(method)

  if (!is.data.frame(data)) stop("`data` must be a data.frame.")
  if (!is.character(score_col) || length(score_col) != 1) stop("`score_col` must be a single column name.")
  if (!score_col %in% colnames(data)) stop("`score_col` not found in `data`.")

  score <- data[[score_col]]
  if (!is.numeric(score)) stop("`score_col` must be numeric.")
  if (anyNA(score)) stop("Score contains NA; handle missing values first.")
  n <- nrow(data)

  # sample IDs
  if (!is.null(id_col)) {
    if (!id_col %in% colnames(data)) stop("`id_col` not found in `data`.")
    sample_id <- data[[id_col]]
  } else {
    sample_id <- seq_len(n)
  }

  # determine group sizes
  if (!is.null(group_sizes)) {
    group_sizes <- as.integer(group_sizes)
    if (sum(group_sizes) != n) stop("Sum of `group_sizes` must equal nrow(data).")
    if (any(group_sizes <= 0)) stop("All `group_sizes` must be positive.")
    K_used <- length(group_sizes)
  } else {
    if (is.null(K)) stop("Provide either `K` or `group_sizes`.")
    K_used <- as.integer(K)
    base <- n %/% K_used
    r <- n %% K_used
    if (!allow_unequal && r != 0) stop("n not divisible by K; set allow_unequal=TRUE.")
    group_sizes <- rep(base, K_used)
    if (allow_unequal && r > 0) group_sizes[seq_len(r)] <- group_sizes[seq_len(r)] + 1
  }

  core <- balance_partition_core(
    score = score,
    group_sizes = group_sizes,
    lambda = lambda,
    B = B,
    method = method,
    seed = seed
  )

  assignment <- data.frame(
    sample_id = sample_id,
    group = core$groups,
    score = score
  )
  assignment <- assignment[order(assignment$group), , drop = FALSE]

  group_stats <- aggregate(score ~ group, assignment, function(x) {
    c(n = length(x), mean = mean(x), sd = stats::sd(x))
  })

  plot_obj <- NULL
  if (isTRUE(output_plot)) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("output_plot=TRUE requires package 'ggplot2'.")
    }
    assignment$group <- factor(assignment$group, levels = sort(unique(assignment$group)))

    plot_obj <- ggplot2::ggplot(assignment, ggplot2::aes(x = .data$group, y = .data$score)) +
      ggplot2::geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1) +
      ggplot2::stat_summary(
        fun = "mean",
        geom = "errorbar",
        ggplot2::aes(ymax = after_stat(y), ymin = after_stat(y)),
        width = 0.6,
        linetype = "dashed"
      ) +
      ggplot2::stat_summary(
        fun.data = "mean_sdl",
        fun.args = list(mult = 1),
        geom = "pointrange"
      ) +
      ggplot2::labs(
        title = paste0("Balanced partition: ", score_col),
        subtitle = paste0(
          "K=", length(core$group_sizes),
          ", lambda=", lambda,
          ", B=", B,
          ", method=", method,
          ", loss=", signif(core$loss, 4)
        ),
        x = "Group",
        y = score_col
      )
  }

  files <- NULL
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    files <- list()

    if (isTRUE(output_csv)) {
      out_df <- data
      out_df$.__group <- core$groups
      out_df$.__score <- score
      csv_path <- file.path(output_dir, paste0(file_prefix, "_grouped.csv"))
      utils::write.csv(out_df, csv_path, row.names = FALSE)
      files$csv <- csv_path
    }

    if (isTRUE(output_plot)) {
      png_path <- file.path(output_dir, paste0(file_prefix, "_plot.png"))
      ggplot2::ggsave(png_path, plot_obj, device = "png")
      files$png <- png_path
    }

    if (length(files) == 0) files <- NULL
  }

  list(
    assignment = assignment,
    group_stats = group_stats,
    loss = core$loss,
    group_sizes = core$group_sizes,
    plot = plot_obj,
    files = files,
    params = list(
      score_col = score_col,
      K = length(core$group_sizes),
      group_sizes = core$group_sizes,
      id_col = id_col,
      lambda = lambda,
      B = B,
      method = method,
      allow_unequal = allow_unequal,
      seed = seed,
      output_dir = output_dir,
      file_prefix = file_prefix,
      output_csv = output_csv,
      output_plot = output_plot
    )
  )
}
