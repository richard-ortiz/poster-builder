# ---------------------------------------------------------------------------
# References (APA 7th edition) from DOIs, via Crossref.
# The Crossref lookup and APA author/initials helpers follow the Ortiz Lab
# CV Builder, so both apps format references the same way.
# A reference block holds editable text (one reference per line, using the
# app's *italic* markup); "Fill in from DOIs" appends formatted references.
# ---------------------------------------------------------------------------

clean_doi <- function(d) trimws(sub("^(https?://(dx\\.)?doi\\.org/|doi:\\s*)", "", trimws(d %||% ""), ignore.case = TRUE))

# APA initials, e.g. "Richard Joaquin" -> "R. J.", "Jean-Luc" -> "J.-L."
apa_initials <- function(given) {
  parts <- strsplit(trimws(gsub("\\.", " ", given)), "\\s+")[[1]]
  parts <- parts[nzchar(parts)]
  paste(vapply(parts, function(p) paste0(paste0(substr(strsplit(p, "-")[[1]], 1, 1), "."), collapse = "-"), ""), collapse = " ")
}
apa_author_list <- function(a) {
  n <- length(a); if (n == 0) return("")
  if (n == 1) return(a)
  if (n == 2) return(paste0(a[1], ", & ", a[2]))
  if (n <= 20) return(paste0(paste(a[-n], collapse = ", "), ", & ", a[n]))
  paste0(paste(a[1:19], collapse = ", "), ", . . . ", a[n])
}

# Crossref "message" record -> fields
crossref_fields <- function(res) {
  au <- vapply(res$author %||% list(), function(a) {
    if (!is.null(a$family)) paste0(a$family, if (!is.null(a$given)) paste0(", ", apa_initials(a$given)) else "") else a$name %||% ""
  }, "")
  dp <- res$issued$`date-parts`[[1]][[1]] %||% res$published$`date-parts`[[1]][[1]] %||% ""
  strip <- function(x) trimws(gsub("\\s+", " ", gsub("<[^>]+>", "", unlist(x)[1] %||% "")))
  list(authors = apa_author_list(au[nzchar(au)]), title = strip(res$title), journal = strip(res$`container-title`),
       volume = as.character(res$volume %||% ""), issue = as.character(res$issue %||% ""),
       pages = as.character(res$page %||% res$`article-number` %||% ""), year = as.character(dp))
}

crossref_lookup <- function(doi, timeout = 10) {
  old <- options(timeout = timeout); on.exit(options(old))
  tryCatch(jsonlite::fromJSON(paste0("https://api.crossref.org/works/", utils::URLencode(doi, reserved = FALSE)),
                              simplifyVector = FALSE)$message, error = function(e) NULL)
}

# One APA 7 journal-article reference as a line of the app's markup
# (*...* = italic). Asterisks in titles are escaped so they print literally.
apa_reference_line <- function(x, doi) {
  esc <- function(t) gsub("*", "\\*", t, fixed = TRUE)
  end_punct <- function(t) if (grepl("[.?!]$", t)) t else paste0(t, ".")
  out <- character(0)
  if (nzchar(x$authors)) out <- c(out, esc(x$authors), " ")
  out <- c(out, sprintf("(%s). ", if (nzchar(x$year)) x$year else "n.d."))
  if (nzchar(x$title)) out <- c(out, esc(end_punct(x$title)), " ")
  if (nzchar(x$journal)) {
    out <- c(out, "*", esc(x$journal), if (nzchar(x$volume)) paste0(", ", x$volume) else "", "*")
    if (nzchar(x$issue)) out <- c(out, "(", x$issue, ")")
    if (nzchar(x$pages)) out <- c(out, ", ", gsub("-", "–", x$pages))
    out <- c(out, ". ")
  }
  out <- c(out, "https://doi.org/", doi)
  paste0(out, collapse = "")
}

# Split the DOI box into clean DOIs (one per line; spaces/commas also accepted)
parse_doi_list <- function(txt) {
  d <- unlist(strsplit(gsub("\r", "", txt %||% ""), "[\n,;]+|\\s{2,}"))
  d <- vapply(d, clean_doi, "", USE.NAMES = FALSE)
  unique(d[nzchar(d) & grepl("^10\\.", d)])
}

# Reference lines currently in the block (blank lines dropped), optionally sorted A-Z
reference_lines <- function(b) {
  l <- trimws(strsplit(gsub("\r", "", b$text %||% ""), "\n", fixed = TRUE)[[1]])
  l <- l[nzchar(l)]
  if (!identical(b$sort, FALSE)) l <- l[order(tolower(gsub("[^[:alnum:] ]", "", l)))]
  l
}

# Gentle checks shown under the block, like the CV builder's tips
reference_tips <- function(b) {
  l <- reference_lines(b); tips <- character(0)
  if (!length(l)) return(tips)
  dois <- tolower(regmatches(l, regexpr("10\\.[^ ]+$", l)))
  if (any(duplicated(dois))) tips <- c(tips, "The same DOI appears more than once.")
  nodoi <- !grepl("doi\\.org/10\\.", l)
  if (any(nodoi)) tips <- c(tips, sprintf("%d reference%s without a DOI link. Add one if the source has a DOI.", sum(nodoi), if (sum(nodoi) > 1) "s" else ""))
  # APA uses sentence case for article titles: flag titles with many Capitalised Words
  ttl <- sub("^.*?\\(\\d{4}[a-z]?\\)\\.\\s*|^.*?\\(n\\.d\\.\\)\\.\\s*", "", l)
  ttl <- sub("\\.\\s.*$", "", ttl)
  caps <- vapply(strsplit(ttl, "\\s+"), function(w) { w <- w[-1]; w <- w[nchar(w) > 3]; if (!length(w)) 0 else mean(grepl("^[A-Z][a-z]", w)) }, 0)
  if (any(caps > 0.6)) tips <- c(tips, "APA uses sentence case for article titles: capitalize only the first word, proper nouns, and the word after a colon.")
  if (length(l) > 8) tips <- c(tips, sprintf("%d references is a lot for a poster; 3–6 key sources is typical.", length(l)))
  tips
}
