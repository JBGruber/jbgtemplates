#' Create a new Quarto report file
#'
#' @param name optional name
#'
#' @export
report_template <- function(name = NULL) {
  lines <- readLines(system.file("templates", "skeleton.qmd", package = "jbgtemplates"))
  if (!is.null(name)) {
    lines <- sub("Capitalized Title Here", name, lines, fixed = TRUE)
    f <- paste0(fs::path_sanitize(name), ".qmd")
  } else {
    f <- tempfile("report_", ".", fileext = ".qmd")
  }
  writeLines(lines, f)

  if (rstudio_available()) {
    rstudioapi::documentOpen(f)
  } else {
    utils::file.edit(f)
  }
  invisible()
}

# safely check if rstudioapi is available
rstudio_available <- function() {
  out <- FALSE
  if (rlang::is_installed("rstudioapi")) out <- rstudioapi::isAvailable()
  return(out)
}
