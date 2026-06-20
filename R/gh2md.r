#' Download a GitHub issue and its comments as Markdown#
#'
#' @param repo Repository as URL ("https://github.com/owner/repo/issues/52"),
#'   shorthand ("owner/repo@52"), or "owner/repo" string.
#' @param issue Issue number. Not needed when encoded in `repo`.
#' @param file Optional file path to write the output to.
#' @return Character string of Markdown (invisibly when `file` is given).
#' @examples
#' \dontrun{
#' gh2md("https://github.com/JBGruber/atrrr/issues/52")
#' gh2md("JBGruber/atrrr@52")
#' gh2md("JBGruber/atrrr", 52)
#' gh2md("JBGruber/atrrr@52", file = "52.md")
#' }
#'
#' @export
gh2md <- function(repo, issue, file) {
  # parse the various input formats into owner, repo, issue number
  if (missing(issue)) {
    if (grepl("^https://github\\.com/", repo)) {
      parts <- regmatches(
        repo,
        regexpr("github\\.com/([^/]+)/([^/]+)/issues/(\\d+)", repo)
      )
      m <- regmatches(
        parts,
        regexec("github\\.com/([^/]+)/([^/]+)/issues/(\\d+)", parts)
      )[[1]]
      owner <- m[2]
      repo_name <- m[3]
      issue <- as.integer(m[4])
    } else if (grepl("@", repo)) {
      parts <- strsplit(repo, "@")[[1]]
      owner <- strsplit(parts[1], "/")[[1]][1]
      repo_name <- strsplit(parts[1], "/")[[1]][2]
      issue <- as.integer(parts[2])
    } else {
      stop(
        "Provide `issue` separately or encode it in `repo` via URL or 'owner/repo@number'."
      )
    }
  } else {
    owner <- strsplit(repo, "/")[[1]][1]
    repo_name <- strsplit(repo, "/")[[1]][2]
    issue <- as.integer(issue)
  }

  # fetch issue body and comments
  issue_data <- gh::gh(
    "/repos/{owner}/{repo}/issues/{issue}",
    owner = owner,
    repo = repo_name,
    issue = issue
  )
  comment_data <- gh::gh(
    "/repos/{owner}/{repo}/issues/{issue}/comments",
    owner = owner,
    repo = repo_name,
    issue = issue,
    .limit = Inf
  )

  fmt_timestamp <- function(ts) {
    format(
      as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      "%Y-%m-%d %H:%M UTC"
    )
  }

  # build each block: original post + comments
  blocks <- c(
    list(list(
      user = issue_data$user$login,
      timestamp = issue_data$created_at,
      body = issue_data$body %||% ""
    )),
    lapply(comment_data, function(c) {
      list(
        user = c$user$login,
        timestamp = c$created_at,
        body = c$body %||% ""
      )
    })
  )

  header <- sprintf(
    "# %s\n\nIssue [#%d](%s) in %s/%s\n",
    issue_data$title,
    issue,
    issue_data$html_url,
    owner,
    repo_name
  )

  entries <- vapply(
    blocks,
    function(b) {
      sprintf("## %s, %s\n\n%s", b$user, fmt_timestamp(b$timestamp), b$body)
    },
    character(1)
  )

  md <- paste(c(header, entries), collapse = "\n\n---\n\n")

  if (!missing(file)) {
    writeLines(md, file)
    return(invisible(md))
  }
  md
}

# null-coalescing helper (base R equivalent of rlang::`%||%`)
`%||%` <- function(x, y) if (is.null(x)) y else x
