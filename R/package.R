#' Install packages from CRAN, Bioconductor, or GitHub
#'
#' @description
#' Installs each entry in `pkgs` from the selected `repo`. Skips packages that
#' are already installed (unless `force = TRUE`). For GitHub, pass
#' `"owner/repo"` strings (e.g., `"rstudio/gt"`).
#'
#' @param pkgs Character vector of package identifiers.
#'   - For `repo = "cran"` or `"bioc"`: package names, e.g. `"dplyr"`.
#'   - For `repo = "github"`: `"owner/repo"` strings, e.g. `"tidyverse/ggplot2"`.
#' @param repo One of `"cran"`, `"bioc"`, or `"github"`. Default `"cran"`.
#' @param dependencies Logical; forwarded to `utils::install.packages()` and
#'   `remotes::install_github()`. Default `TRUE`.
#' @param update Logical; only used when `repo = "bioc"`. If `TRUE`, allow
#'   `BiocManager::install()` to update other Bioconductor packages. Default `FALSE`
#'   to keep installs reproducible.
#' @param force Logical; reinstall even if the package appears installed.
#'   (For GitHub, the installed check uses the package name part after `/`.)
#'
#' @return Character vector of packages that are still not installed afterward.
#' @export
#'
#' @importFrom stats na.omit
#'
#' @examples
#' \dontrun{
#'   install_packages(c("dplyr","ggplot2"))                    # CRAN
#'   install_packages("SummarizedExperiment", repo = "bioc")   # Bioconductor
#'   install_packages("rstudio/gt", repo = "github")           # GitHub
#' }
install_packages <- function(pkgs,
                             repo = c("cran", "bioc", "github"),
                             dependencies = TRUE,
                             update = FALSE,
                             force = FALSE) {
  stopifnot(is.character(pkgs))
  repo <- match.arg(repo)

  # Normalize inputs
  pkgs <- unique(stats::na.omit(pkgs))

  # Ensure optional helpers are available
  if (repo == "bioc" && !requireNamespace("BiocManager", quietly = TRUE)) {
    utils::install.packages("BiocManager")
  }
  if (repo == "github" && !requireNamespace("remotes", quietly = TRUE)) {
    utils::install.packages("remotes")
  }

  # Determine the installed-check names
  check_names <- if (repo == "github") basename(pkgs) else pkgs

  for (i in seq_along(pkgs)) {
    pkg_id   <- pkgs[i]
    pkg_name <- check_names[i]

    # Skip if installed (unless force)
    if (!force && repo != "github" && requireNamespace(pkg_name, quietly = TRUE)) next
    if (!force && repo == "github" && requireNamespace(pkg_name, quietly = TRUE)) next

    # Install by source
    tryCatch(
      {
        if (repo == "cran") {
          utils::install.packages(pkg_name, dependencies = dependencies)
        } else if (repo == "bioc") {
          BiocManager::install(pkg_name, ask = FALSE, update = update)
        } else { # github
          remotes::install_github(pkg_id, dependencies = dependencies, upgrade = "never")
        }
      },
      error = function(e) {
        message(sprintf("[%s] failed: %s (%s)", toupper(repo), pkg_id, e$message))
      },
      warning = function(w) {
        message(sprintf("[%s] warning for %s: %s", toupper(repo), pkg_id, w$message))
      }
    )
  }

  # After attempting installation, report anything still missing
  still_missing <- check_packages(check_names)
  if (length(still_missing) > 0) {
    message("The following packages are still missing: ",
            paste(still_missing, collapse = ", "))
  }
  still_missing
}

#' Check which packages are not installed
#'
#' @param pkgs Character vector of package names.
#' @return Character vector of packages not currently available.
#' @export
#'
#' @examples
#' check_packages(c("stats","definitely_not_a_real_pkg"))
check_packages <- function(pkgs) {
  stopifnot(is.character(pkgs))
  pkgs <- unique(stats::na.omit(pkgs))
  pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
}
