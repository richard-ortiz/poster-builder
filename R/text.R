# ---------------------------------------------------------------------------
# Rich text: a tiny markdown-like syntax students can type into text boxes.
#   **bold**   *italic*   ^superscript^   ~subscript~   lines starting "- " are bullets
#   Use \* \^ \~ for a literal symbol.
# Text is measured with Arial-compatible font metrics so boxes can be auto-fit.
# ---------------------------------------------------------------------------

HANG_IN <- 0.4        # hanging indent for reference lists (inches)
LINE_HEIGHT <- 1.2     # line pitch in em (PowerPoint single spacing for Arial ~1.15)
PARA_SPACE  <- 0.35    # space after each paragraph, in em
FIT_SAFETY  <- 0.96    # measured widths are inflated slightly so PowerPoint never wraps sooner than we predict

# Paragraph rules (so text pasted from a PDF with hard line breaks reflows nicely):
#   - a blank line starts a new paragraph
#   - a line starting with a bullet ("- ", "• ") or a number ("1. ", "2) ") starts a new paragraph
#   - any other line break is joined with a space, unless keep_lines = TRUE
#     (every line is then its own paragraph, e.g. for a reference list)
parse_rich <- function(text, keep_lines = FALSE) {
  if (is.null(text) || !nzchar(trimws(text))) return(list())
  text <- gsub("\r", "", text, fixed = TRUE)
  text <- gsub("\\*", "\001", text, fixed = TRUE)
  text <- gsub("\\^", "\002", text, fixed = TRUE)
  text <- gsub("\\~", "\003", text, fixed = TRUE)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  paras <- list(); cur <- NULL; cur_bullet <- FALSE
  flush <- function() {
    if (!is.null(cur) && nzchar(trimws(cur)))
      paras[[length(paras) + 1]] <<- list(bullet = cur_bullet, runs = parse_runs(trimws(cur)))
    cur <<- NULL; cur_bullet <<- FALSE
  }
  for (ln in lines) {
    if (!nzchar(trimws(ln))) { flush(); next }
    bullet <- grepl("^\\s*[-•*]\\s+", ln)
    numbered <- grepl("^\\s*\\(?[0-9]{1,3}[.)]\\s+", ln)
    # a capitalised line right after a bullet, or any line after one ending in ":", starts fresh
    after_bullet <- cur_bullet && grepl("^[A-Z0-9]", ln)
    after_colon <- !is.null(cur) && grepl(":\\s*$", cur)
    if (bullet || numbered || keep_lines || is.null(cur) || after_bullet || after_colon) {
      flush()
      if (bullet) ln <- sub("^\\s*[-•*]\\s+", "", ln)
      cur <- ln; cur_bullet <- bullet
    } else {
      # join the wrapped line; a line ending in "-" ("open-\nsource") joins without a space
      cur <- if (grepl("[A-Za-z]-$", cur)) paste0(cur, trimws(ln)) else paste(cur, trimws(ln))
    }
  }
  flush()
  paras
}

parse_runs <- function(s) {
  m <- gregexpr("\\*\\*|\\*|\\^|~", s)[[1]]
  st <- list(b = FALSE, i = FALSE, sup = FALSE, sub = FALSE)
  runs <- list(); pos <- 1
  add <- function(txt) {
    if (!nzchar(txt)) return()
    txt <- chartr("\001\002\003", "*^~", txt)
    runs[[length(runs) + 1]] <<- c(list(t = txt), st)
  }
  if (m[1] != -1) {
    lens <- attr(m, "match.length")
    for (k in seq_along(m)) {
      add(substr(s, pos, m[k] - 1))
      tok <- substr(s, m[k], m[k] + lens[k] - 1)
      if (tok == "**") st$b <- !st$b
      else if (tok == "*") st$i <- !st$i
      else if (tok == "^") { st$sup <- !st$sup; st$sub <- FALSE }
      else if (tok == "~") { st$sub <- !st$sub; st$sup <- FALSE }
      pos <- m[k] + lens[k]
    }
  }
  add(substr(s, pos, nchar(s)))
  runs
}

# Plain paragraphs (no markup parsing) – used for headings, captions prefixes etc.
plain_paras <- function(text, b = FALSE, i = FALSE) {
  if (is.null(text) || !nzchar(trimws(text))) return(list())
  lapply(strsplit(text, "\n", fixed = TRUE)[[1]], function(ln)
    list(bullet = FALSE, runs = list(list(t = ln, b = b, i = i, sup = FALSE, sub = FALSE))))
}

# Make every run in a paragraph list bold / colored etc.
style_paras <- function(paras, ...) {
  mods <- list(...)
  lapply(paras, function(p) { p$runs <- lapply(p$runs, function(r) modifyList(r, mods)); p })
}

# --- measurement -----------------------------------------------------------
char_widths <- function(chars, b, i) {
  tab <- if (b && i) FONT_W_BI else if (b) FONT_W_B else if (i) FONT_W_I else FONT_W_R
  w <- tab[match(chars, FONT_CHARS)]
  w[is.na(w)] <- 0.6
  w
}

# Word widths (in em at 1 pt) for a paragraph, memoised by its content, so
# re-measuring the same text at many sizes/widths (auto-adjust) is cheap.
.WORD_MEMO <- new.env(hash = TRUE)
para_words <- function(p) {
  key <- paste0(isTRUE(p$bullet), "|", paste(vapply(p$runs, function(r)
    paste0(r$t, "\r", isTRUE(r$b), isTRUE(r$i), isTRUE(r$sup) || isTRUE(r$sub)), ""), collapse = "\n"))
  hit <- .WORD_MEMO[[key]]
  if (!is.null(hit)) return(hit)
  ws <- numeric(0); sp <- logical(0)
  for (r in p$runs) {
    ch <- strsplit(r$t, "")[[1]]
    if (!length(ch)) next
    scale <- if (isTRUE(r$sup) || isTRUE(r$sub)) 0.67 else 1
    ws <- c(ws, char_widths(ch, isTRUE(r$b), isTRUE(r$i)) * scale)
    sp <- c(sp, ch == " ")
  }
  words <- if (length(ws)) { rl <- rle(sp); ends <- cumsum(rl$lengths); starts <- ends - rl$lengths + 1
    vapply(which(!rl$values), function(k) sum(ws[starts[k]:ends[k]]), numeric(1)) } else numeric(0)
  if (length(.WORD_MEMO) > 20000) rm(list = ls(.WORD_MEMO), envir = .WORD_MEMO)
  assign(key, words, envir = .WORD_MEMO)
  words
}

# Returns number of wrapped lines for one paragraph at `size` pt within `width` inches
para_lines <- function(p, size, width) {
  words <- para_words(p)
  if (!length(words)) return(1)
  k <- size / 72 / FIT_SAFETY
  avail <- width - (if (isTRUE(p$bullet)) size * 1.1 / 72 else if (isTRUE(p$hang)) HANG_IN else 0)
  if (avail <= 0) return(99)
  ww <- words * k; space_w <- 0.278 * size / 72
  if (sum(ww) + space_w * (length(ww) - 1) <= avail) return(1)
  lines <- 1; cur <- 0
  for (w in ww) {
    if (cur == 0) {
      if (w > avail) { lines <- lines + ceiling(w / avail) - 1; cur <- w %% avail } else cur <- w
    } else if (cur + space_w + w <= avail) {
      cur <- cur + space_w + w
    } else {
      lines <- lines + 1
      if (w > avail) { lines <- lines + ceiling(w / avail) - 1; cur <- w %% avail } else cur <- w
    }
  }
  lines
}

text_height <- function(paras, size, width) {
  if (!length(paras)) return(0)
  n <- sum(vapply(paras, para_lines, numeric(1), size = size, width = width))
  n * size * LINE_HEIGHT / 72 + (length(paras) - 1) * size * PARA_SPACE / 72
}

# Largest size in [min_size, max_size] (1 pt steps) that fits in width x height
fit_text_size <- function(paras, width, height, max_size, min_size) {
  for (s in seq(max_size, min_size, by = -1)) {
    if (text_height(paras, s, width) <= height) return(list(size = s, fits = TRUE))
  }
  list(size = min_size, fits = FALSE)
}

text_width_1line <- function(paras, size) {
  if (!length(paras)) return(0)
  max(vapply(paras, function(p) {
    sum(unlist(lapply(p$runs, function(r) {
      sc <- if (isTRUE(r$sup) || isTRUE(r$sub)) 0.67 else 1
      sum(char_widths(strsplit(r$t, "")[[1]], isTRUE(r$b), isTRUE(r$i))) * sc
    }))) * size / 72 / FIT_SAFETY
  }, numeric(1)))
}

# --- HTML for the live preview ---------------------------------------------
html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

paras_html <- function(paras, size_px, color = "#000") {
  paste0(vapply(paras, function(p) {
    inner <- paste0(vapply(p$runs, function(r) {
      t <- html_escape(r$t)
      if (isTRUE(r$b)) t <- paste0("<b>", t, "</b>")
      if (isTRUE(r$i)) t <- paste0("<i>", t, "</i>")
      if (isTRUE(r$sup)) t <- paste0("<sup>", t, "</sup>")
      if (isTRUE(r$sub)) t <- paste0("<sub>", t, "</sub>")
      if (!is.null(r$color)) t <- sprintf('<span style="color:#%s">%s</span>', r$color, t)
      t
    }, character(1)), collapse = "")
    sty <- sprintf("margin:0 0 %.2fpx 0;", size_px * PARA_SPACE)
    if (isTRUE(p$hang))
      return(sprintf('<p style="%spadding-left:%.2fpx;text-indent:-%.2fpx">%s</p>', sty, HANG_IN * PX, HANG_IN * PX, inner))
    if (isTRUE(p$bullet))
      sprintf('<p style="%spadding-left:%.2fpx;text-indent:-%.2fpx">•&nbsp;%s</p>', sty, size_px * 1.1, size_px * 1.1, inner)
    else sprintf('<p style="%s">%s</p>', sty, inner)
  }, character(1)), collapse = "")
}
