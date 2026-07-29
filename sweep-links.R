#!/usr/bin/env Rscript

## sweep-links.R: post-process a built pkgdown site, removing rdrr.io
## fallback links that downlit inserts for packages without a pkgdown
## site (base R most of all).
##
## Policy:
##   - default: UNWRAP. The <a> is removed, the code text stays bare.
##     No external target means nothing to rot.
##   - allowlist: entries like ## "graphics::image", are instead REWRITTEN to
##     the official R manuals at search.r-project.org, validated with a
##     HEAD request at build time. A target that does not validate (or
##     can't be checked, e.g. offline) falls back to UNWRAP, never to
##     rdrr. Rot is bounded by the rebuild cadence.
##
## Usage:
##   Rscript tools/sweep-links.R <docs-dir> [--no-validate]
##
## In a pkgdown GitHub Actions workflow, add after the build step and
## before deploy:
##
##   - name: Sweep fallback links
##     run: Rscript tools/sweep-links.R docs
##
## Base R only, no dependencies. ASCII only.

ALLOWLIST <- c(
  ## "graphics::image",
  ## "graphics::rasterImage"
)

## map an rdrr URL to the official manuals, or NA if no mapping known
official_url <- function(url) {
  ## base packages: https://rdrr.io/r/<pkg>/<topic>.html
  m <- regmatches(url, regexec("^https://rdrr\\.io/r/([^/]+)/([^/]+)\\.html", url))[[1]]
  if (length(m) == 3L) {
    return(sprintf("https://search.r-project.org/R/refmans/%s/html/%s.html", m[2], m[3]))
  }
  ## CRAN packages: https://rdrr.io/pkg/<pkg>/man/<topic>.html
  m <- regmatches(url, regexec("^https://rdrr\\.io/pkg/([^/]+)/man/([^/]+)\\.html", url))[[1]]
  if (length(m) == 3L) {
    return(sprintf("https://search.r-project.org/CRAN/refmans/%s/html/%s.html", m[2], m[3]))
  }
  NA_character_
}

## "pkg::topic" key for allowlist matching, or NA
url_key <- function(url) {
  m <- regmatches(url, regexec(
    "^https://rdrr\\.io/(?:r/([^/]+)|pkg/([^/]+)/man)/([^/]+)\\.html", url))[[1]]
  if (length(m) == 4L) {
    pkg <- if (nzchar(m[2])) m[2] else m[3]
    return(paste0(pkg, "::", m[4]))
  }
  NA_character_
}

## memoized HEAD-request validation, fail-open to FALSE
.ok <- new.env(parent = emptyenv())
url_valid <- function(url, validate = TRUE) {
  if (!validate) return(TRUE)
  if (is.null(.ok[[url]])) {
    .ok[[url]] <- tryCatch({
      h <- curlGetHeaders(url, timeout = 10)
      isTRUE(attr(h, "status") == 200L)
    }, error = function(e) FALSE, warning = function(w) FALSE)
  }
  .ok[[url]]
}

sweep_file <- function(path, validate = TRUE) {
  x <- paste(readLines(path, warn = FALSE), collapse = "\n")
  ## downlit/pkgdown anchors: attributes vary (class="external-link",
  ## rel=...), anchors do not nest, inner text is the code text
  pattern <- '<a[^>]*\\bhref="(https://rdrr\\.io/[^"]+)"[^>]*>(.*?)</a>'
  m <- gregexpr(pattern, x, perl = TRUE)
  hits <- regmatches(x, m)[[1]]
  if (length(hits) == 0L) return(c(unwrapped = 0L, swapped = 0L))

  counts <- c(unwrapped = 0L, swapped = 0L)
  replacement <- vapply(hits, function(a) {
    url   <- sub(pattern, "\\1", a, perl = TRUE)
    inner <- sub(pattern, "\\2", a, perl = TRUE)
    key   <- url_key(url)
    if (!is.na(key) && key %in% ALLOWLIST) {
      target <- official_url(url)
      if (!is.na(target) && url_valid(target, validate)) {
        counts["swapped"] <<- counts["swapped"] + 1L
        return(sprintf('<a href="%s" class="external-link">%s</a>', target, inner))
      }
    }
    ## default policy, and the fallback for everything else: bare text
    counts["unwrapped"] <<- counts["unwrapped"] + 1L
    inner
  }, character(1L))

  regmatches(x, m) <- list(replacement)
  writeLines(x, path)
  counts
}

sweep_site <- function(docs = "docs", validate = TRUE) {
  files <- list.files(docs, pattern = "\\.html$", recursive = TRUE,
                      full.names = TRUE)
  if (length(files) == 0L) stop("no html files found under '", docs, "'")
  total <- c(unwrapped = 0L, swapped = 0L)
  for (f in files) {
    n <- sweep_file(f, validate = validate)
    if (sum(n) > 0L) {
      message(sprintf("%s: unwrapped %d, swapped %d", f, n[1], n[2]))
    }
    total <- total + n
  }
  message(sprintf("sweep: %d links unwrapped, %d swapped across %d files",
                  total[1], total[2], length(files)))
  if (sum(total) == 0L) {
    warning("no rdrr.io links found anywhere - if downlit output is ",
            "expected here, its URL scheme may have changed and this ",
            "sweep needs updating", call. = FALSE)
  }
  invisible(total)
}

if (sys.nframe() == 0L && !interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  docs <- if (length(args) >= 1L && !startsWith(args[1], "--")) args[1] else "docs"
  validate <- !"--no-validate" %in% args
  sweep_site(docs, validate = validate)
}
