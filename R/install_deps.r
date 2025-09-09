#' Install dependencies
#'
#' Checks the path for R, Rmd and Qmd files and installs all references packages.
#'
#' @param path path to check for files (checks recursivly).
#' @param packages additional packages.
#' @param gh_packages additional GitHub packages.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' install_deps(gh_packages = "JBGruber/paperboy")
#' }
install_deps <- function(path = ".",
                         pattern = "*.[.](Rmd|rmd|qmd)$",
                         recursive = TRUE,
                         packages = NULL,
                         gh_packages = NULL,
                         dependencies = TRUE) {
  rlang::check_installed("attachment", "in order to use this function")
  files <- list.files(
    path = path,
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  # the pattern matching of list.files is not great
  files <- grep(pattern = pattern, x = files, value = TRUE, perl = TRUE)
  files <- split(files, tolower(tools::file_ext(files)))

  needed_packages <- unique(c(
    if (!is.null(files$rmd)) attachment::att_from_rmds(files$rmd),
    if (!is.null(files$qmd)) attachment::att_from_qmds(files$qmd),
    if (!is.null(files$r))  attachment::att_from_rscripts(files$r),
    packages
  ))

  if (rlang::is_installed("pak")) {
    action <- function(pkgs, ...) pak::pak(pkgs, dependencies = dependencies)
    if (!is.null(gh_packages)) action(gh_packages)
  } else {
    action <- function(pkgs, ...) install.packages(pkgs, dependencies = dependencies)
    for (p in gh_packages) install_deps_gh(p)
  }

  rlang::check_installed(unique(needed_packages), action = action)
}

install_deps_gh <- function(package) {
  if (!rlang::is_installed(basename(package))) remotes::install_github(package)
}
