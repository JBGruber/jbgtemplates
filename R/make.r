#' make a project containing quarto document
#'
#' A Make-like pipeline function that checks if source files have been updated
#' and renders them if the html output is older than the source file.
#' \code{files} are made in the order they are supplied and the function checks
#' if each file's output is newer than the last and whether the output is newer
#' than the source file.
#'
#' @param files the quarto source files.
#' @param destfiles optional destination files. Names are created from input and
#'   output_ext if NULL.
#' @param output_ext alternatively to destfiles you can supply an output
#'   extension
#' @param ... passed on to \link[quarto]{quarto_render}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' make_quarto(list.files(ist.files(pattern = "\\.qmd$")))
#' }
make_quarto <- function(files,
                        destfiles = NULL,
                        output_ext = ".html",
                        ...) {

  if (is.null(destfiles)) destfiles <- paste0(tools::file_path_sans_ext(files), output_ext)

  t <- file.info(destfiles[1])$ctime

  for (i in seq_along(files)) {
    if (
      # render file if source is newer than destination
      isTRUE(file.info(files[i])$ctime >= file.info(destfiles[i])$ctime) ||
      # or if output is NOT newer than previous destination file
      !isTRUE(file.info(destfiles[i])$ctime >= t)
    ) {
      cli::cli_progress_step("Making {destfiles[i]}.")
      quarto::quarto_render(
        input = files[i],
        output_file = destfiles[i]
      )
      cli::cli_progress_done()
    } else {
      cli::cli_inform(c("v" = "{destfiles[i]} already up to date."))
    }
    t <- file.info(destfiles[i])$ctime
  }

}

