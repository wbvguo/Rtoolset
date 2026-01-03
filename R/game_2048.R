#' Play the 2048 game in the R console
#'
#' Use WASD keys to move tiles.
#'
#' @param board_size Board size (default 4)
#' @param target Winning tile value (default 2048)
#'
#' @return Final board (invisibly)
#' @export
play_2048 <- function(board_size = 4, target = 2048) {

  # -----------------------------
  # Collapse a vector (2048 rule)
  # -----------------------------
  collapse_vec <- function(x) {
    n <- length(x)
    new <- x[x > 0]

    i <- 1L
    while (i < length(new)) {
      if (new[i] == new[i + 1L]) {
        new[i] <- new[i] * 2L
        new <- new[-(i + 1L)]
      }
      i <- i + 1L
    }

    c(new, integer(n - length(new)))
  }

  # -----------------------------
  # Initialize board
  # -----------------------------
  init_board <- function(n) {
    m <- matrix(0L, n, n)
    idx <- sample(n * n, 2)
    m[idx] <- 2L
    m
  }

  # -----------------------------
  # Apply move
  # -----------------------------
  apply_move <- function(m, move) {
    switch(
      move,
      "w" = apply(m, 2, collapse_vec),                          # up
      "a" = t(apply(m, 1, collapse_vec)),                       # left
      "s" = apply(m[nrow(m):1, ], 2, collapse_vec)[nrow(m):1,], # down
      "d" = t(apply(m[, nrow(m):1], 1, collapse_vec))[, nrow(m):1], # right
      m
    )
  }

  # -----------------------------
  # Plot board
  # -----------------------------
  plot_board <- function(m) {
    n <- nrow(m)

    grid <- expand.grid(1:n, 1:n)
    idx  <- arrayInd(which(m > 0), dim(m))

    if (nrow(idx) == 0) {
      df <- data.frame(V1 = integer(), V2 = integer(), value = factor())
    } else {
      df <- cbind(
        as.data.frame(idx),
        value = factor(m[m > 0], levels = 2^(1:11))
      )
    }

    ggplot2::ggplot(df, ggplot2::aes(V2, n + 1L - V1)) +
      ggplot2::geom_tile(
        data = grid,
        ggplot2::aes(Var1, Var2),
        fill = "grey90",
        width = 0.9,
        height = 0.9
      ) +
      ggplot2::geom_tile(
        ggplot2::aes(fill = value),
        width = 0.9,
        height = 0.9
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = value),
        colour = "white",
        size = 10
      ) +
      ggplot2::theme_void() +
      ggplot2::coord_equal() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_fill_viridis_d(drop = FALSE, end = 0.9)
  }

  # -----------------------------
  # Game loop
  # -----------------------------
  m <- init_board(board_size)
  m_prior <- m

  print(plot_board(m))

  while (max(m) < target && any(m == 0)) {

    if (!identical(m, m_prior)) {
      m[sample(which(m == 0), 1)] <- 2L
      print(plot_board(m))
    }

    m_prior <- m
    move <- readline(prompt = "move (w/a/s/d, q to quit): ")

    if (move == "q") break
    m <- apply_move(m, move)
  }

  if (max(m) >= target) {
    message("You won!")
  }

  invisible(m)
}

