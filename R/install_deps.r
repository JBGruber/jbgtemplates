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
                         packages = NULL,
                         gh_packages = NULL) {
  rlang::check_installed("attachment", "in order to use this function")
  files <- list.files(path, pattern = ".R$|.Rmd$|.Qmd", full.names = TRUE,
                      recursive = TRUE, ignore.case = TRUE)
  files <- split(files, tolower(tools::file_ext(files)))

  needed_packages <- c(
    if (!is.null(files$rmd)) attachment::att_from_rmds(files$rmd),
    if (!is.null(files$qmd)) attachment::att_from_qmds(files$qmd),
    if (!is.null(files$r))  attachment::att_from_rscripts(files$r),
    packages
  )
  rlang::check_installed(unique(needed_packages))
  for (p in gh_packages) install_deps_gh(p)
}

install_deps_gh <- function(package) {
  if (!rlang::is_installed(basename(package))) remotes::install_github(package)
}
