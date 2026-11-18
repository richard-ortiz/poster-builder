# ---------------------------------------------------------------------------
# Poster geometry (inches) for the NIU 40 x 32 template, layouts, and the
# block-stacking engine.  Every column is a vertical stack of panels; each
# panel holds an ordered list of blocks (text, figure, figure row, table,
# callout, QR code, equation).  Blocks with a "size" take that % of the
# panel width; text fills what is left and its font shrinks to fit.
# ---------------------------------------------------------------------------

POSTER_W <- 32; POSTER_H <- 40
NIU_RED  <- "C8102E"
CONTENT_TOP <- 6.35; CONTENT_BOTTOM <- 39.05
PANEL_GAP <- 0.6
PAD_X <- 0.55; PAD_BOTTOM <- 0.45
HEAD_TOP <- 0.2; HEAD_AFTER <- 0.35
BLOCK_GAP <- 0.3
NIU_LOGO <- list(x = 25.84, y = 1.15, w = 4.89, h = 1.78)

SECTION_KEYS <- c("intro", "goal", "methods", "results", "conclusion", "ack", "refs")
SECTION_DEFAULT_HEADINGS <- c(intro = "Introduction", goal = "Research Goal", methods = "Methods",
  results = "Figures and Results", conclusion = "Conclusion", ack = "Acknowledgements", refs = "References")

# A layout is a list of columns. Each column: x, w, and a list of slots.
# A slot is either one section key or a row of keys (side-by-side panels).
# `frac` gives the default (unbalanced) share of the column height.
LAYOUTS <- list(
  classic = list(
    label = "Classic 3-column (template)",
    cols = list(
      list(x = 0.94,  w = 7.33,  slots = list("intro", "goal", "methods"),          frac = c(12.7, 4.55, 14.2)),
      list(x = 8.94,  w = 13.86, slots = list("results"),                           frac = 1),
      list(x = 23.59, w = 7.42,  slots = list("conclusion", "ack", "refs"),         frac = c(17.5, 6.7, 7.3))
    )),
  wide = list(
    label = "Wide results (2 columns)",
    cols = list(
      list(x = 0.94,  w = 9.2,   slots = list("intro", "goal", "methods"),          frac = c(11, 5, 16.5)),
      list(x = 10.74, w = 20.32, slots = list("results", c("conclusion", "ack", "refs")), frac = c(22, 10))
    )),
  methods_mid = list(
    label = "Methods in the middle (methods-heavy posters)",
    cols = list(
      list(x = 0.94,  w = 7.33,  slots = list("intro", "goal"),                     frac = c(20, 11)),
      list(x = 8.94,  w = 13.86, slots = list("methods"),                           frac = 1),
      list(x = 23.59, w = 7.42,  slots = list("results", "conclusion", "ack", "refs"), frac = c(14, 9, 4, 5))
    )),
  four = list(
    label = "Four columns",
    cols = list(
      list(x = 0.94,  w = 7.07,  slots = list("intro", "goal"),                     frac = c(22, 10)),
      list(x = 8.61,  w = 7.07,  slots = list("methods"),                           frac = 1),
      list(x = 16.28, w = 7.07,  slots = list("results"),                           frac = 1),
      list(x = 23.95, w = 7.07,  slots = list("conclusion", "ack", "refs"),         frac = c(15, 7.5, 9))
    ))
)

# ---- block measurement ----------------------------------------------------
# Readability floors, based on accepted PURC posters (24 pt body, 17 pt references)
MIN_BODY <- 20      # body text never goes below this
MIN_SMALL <- 17     # captions, tables, flow-chart and label text never go below this
caption_size <- function(body) max(round(body * 0.85), MIN_SMALL)

block_is_empty <- function(b, assets) {
  switch(b$type,
    text = , callout = !nzchar(trimws(b$text %||% "")),
    figure = is.null(assets[[b$asset %||% ""]]),
    figrow = , figpair = !length(Filter(function(a) !is.null(assets[[a]]), unlist(b$assets))),
    table = is.null(table_cells(b, assets)),
    qr = !nzchar(trimws(b$url %||% "")),
    equation = !nzchar(trimws(b$latex %||% "")),
    subheading = !nzchar(trimws(b$text %||% "")),
    refs = !length(reference_lines(b)),
    flow = !length(flow_steps(b)),
    figtext = is.null(assets[[b$asset %||% ""]]) && !nzchar(trimws(b$text %||% "")),
    TRUE)
}

# Steps are one per line. If the text contains blank lines, steps are separated by
# blank lines instead, so a single step can span several lines.
flow_steps <- function(b) {
  txt <- gsub("\r", "", b$steps %||% "")
  st <- if (grepl("\n[ \t]*\n", txt)) gsub("\\s*\n\\s*", " ", strsplit(txt, "\n[ \t]*\n")[[1]]) else strsplit(txt, "\n")[[1]]
  st <- trimws(st); st[nzchar(st)]
}
SIZE_RANGE <- list(figure = c(20, 100), figrow = c(30, 100), figpair = c(30, 100), table = c(30, 100), qr = c(15, 70),
                   equation = c(50, 250), figtext = c(20, 100))
BOX_INSET <- 0.2

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

caption_paras <- function(b, label) {
  cap <- trimws(b$caption %||% "")
  if (is.null(label) && !nzchar(cap)) return(list())
  p <- parse_rich(cap)
  lead <- list(t = paste0(label, if (nzchar(cap)) " " else ""), b = TRUE, i = FALSE, sup = FALSE, sub = FALSE)
  if (!length(p)) p <- list(list(bullet = FALSE, runs = list()))
  p[[1]]$runs <- c(list(lead), p[[1]]$runs)
  p
}

# Measure one block at body size `s` for inner width `cw`; scale multiplies the
# block's own size setting (used to squeeze oversize figures).
measure_block <- function(b, s, cw, assets, scale = 1, grow = 1) {
  # `grow` (>= 1) lets figures expand into spare space in their panel (auto-adjust)
  if (b$type %in% c("figure", "figrow", "figpair", "figtext") && !identical(b$autogrow, FALSE)) scale <- scale * grow
  pct <- min(1, max(0.15, (b$size %||% 100) / 100 * scale))
  csz <- caption_size(s)
  out <- list(type = b$type, id = b$id, empty = block_is_empty(b, assets))
  if (b$type %in% c("text")) {
    out$paras <- parse_rich(b$text, isTRUE(b$keep_lines)); out$fixed <- FALSE
    out$boxed <- b$boxed %||% "none"
    out$inset <- if (out$boxed == "none") 0 else BOX_INSET
    out$h <- text_height(out$paras, s, cw - 2 * out$inset) + 2 * out$inset
  } else if (b$type == "refs") {
    # APA reference list: one reference per line, hanging indent, A\u2013Z, at the small-text size
    out$size <- caption_size(s); out$fixed <- FALSE
    out$paras <- lapply(reference_lines(b), function(l) { p <- parse_rich(l, TRUE)[[1]]; p$bullet <- FALSE; p$hang <- TRUE; p })
    out$h <- text_height(out$paras, out$size, cw)
  } else if (b$type == "subheading") {
    out$style <- b$style %||% "box"
    out$size <- round(s * 1.25); out$fixed <- FALSE
    out$paras <- style_paras(parse_rich(b$text, TRUE), b = TRUE, i = isTRUE(b$italic %||% TRUE))
    out$inset <- if (out$style == "plain") 0.05 else 0.15
    out$h <- text_height(out$paras, out$size, cw - 2 * out$inset - 0.2) + 2 * out$inset
  } else if (b$type == "flow") {
    steps <- flow_steps(b); n <- length(steps)
    ag <- 0.55; rgap <- 0.5; inset <- 0.15; minw <- 1.6
    fsz <- max(round(s * 0.9), MIN_SMALL)
    paras <- lapply(steps, function(t) style_paras(parse_rich(t, TRUE), b = TRUE))
    box_h <- function(i, w) text_height(paras[[i]], fsz, w - 2 * inset - 0.12) + 2 * inset + 0.08
    # Try every number of rows; split steps as evenly as possible (earlier rows get
    # the extra one), stretch each row's boxes to the full width, and keep the most
    # compact arrangement (least height = least dead space).
    plan_rows <- function(r) {
      counts <- rep(n %/% r, r) + c(rep(1, n %% r), rep(0, r - n %% r))
      if (max(counts) > 1 && (cw - (max(counts) - 1) * ag) / max(counts) < minw) return(NULL)
      idx <- split(seq_len(n), rep(seq_len(r), counts))
      rows <- lapply(seq_len(r), function(ri) {
        m <- counts[ri]; w <- (cw - (m - 1) * ag) / m
        list(ids = idx[[ri]], w = w, h = max(vapply(idx[[ri]], box_h, numeric(1), w = w)))
      })
      list(rows = rows, h = sum(vapply(rows, `[[`, numeric(1), "h")) + rgap * (r - 1))
    }
    best <- NULL
    if (n) for (r in seq_len(n)) {
      pl <- plan_rows(r); if (is.null(pl)) next
      # prefer fewer rows unless more rows is clearly more compact
      if (is.null(best) || pl$h < best$h * 0.92) best <- pl
    }
    boxes <- list(); y <- 0
    if (!is.null(best)) for (ri in seq_along(best$rows)) {
      R <- best$rows[[ri]]; m <- length(R$ids)
      for (j in seq_len(m)) {
        jj <- if (ri %% 2 == 0) m - j + 1 else j      # snake: even rows run right-to-left
        boxes[[length(boxes) + 1]] <- list(dx = (jj - 1) * (R$w + ag), dy = y, w = R$w, h = R$h, paras = paras[[R$ids[j]]], row = ri)
      }
      y <- y + R$h + rgap
    }
    out$fixed <- FALSE; out$boxes <- boxes; out$fsz <- fsz; out$ag <- ag; out$rgap <- rgap; out$inset <- inset
    out$cap <- if (nzchar(trimws(b$caption %||% ""))) parse_rich(b$caption) else list()
    out$capsz <- csz; out$capw <- cw
    out$caph <- if (length(out$cap)) text_height(out$cap, csz, cw) + 0.12 else 0
    out$fh <- if (is.null(best)) 0 else best$h
    out$h <- out$fh + out$caph
  } else if (b$type == "figtext") {
    a <- assets[[b$asset %||% ""]]
    asp <- if (is.null(a)) 4 / 3 else a$w / a$h
    side <- b$side %||% "left"                 # image position: left/right, or text "above"/"below"
    vert <- side %in% c("above", "below")
    fp <- min(if (vert) 1 else 0.75, max(0.2, (b$size %||% 50) / 100 * scale))
    # beside the image, keep a readable text column (~2.6 in or 35% of the panel)
    if (!vert && nzchar(trimws(b$text %||% ""))) fp <- max(0.2, min(fp, (cw - 0.3 - max(2.6, 0.35 * cw)) / cw))
    fw <- cw * fp; fh <- fw / asp; gap <- 0.3
    out$side <- side; out$vert <- vert; out$asset <- a; out$fixed <- TRUE
    out$fw <- fw; out$fh <- fh
    out$cap <- caption_paras(b, b$.label); out$capsz <- csz; out$capw <- fw
    out$caph <- if (length(out$cap)) text_height(out$cap, csz, fw) + 0.12 else 0
    out$boxed <- b$boxed %||% "dashed"; out$inset <- if (out$boxed == "none") 0 else BOX_INSET
    out$tw <- if (vert) cw else cw - fw - gap
    out$multi <- identical(b$text_boxes, "each")
    out$box_row <- FALSE
    if (out$multi) {
      items <- flow_steps(list(steps = b$text)); nb <- length(items)
      bgap <- 0.25
      # above/below: boxes side by side in a row when each can be at least 2.4 in wide
      out$box_row <- vert && nb > 1 && (cw - bgap * (nb - 1)) / nb >= 2.4
      bw <- if (out$box_row) (cw - bgap * (nb - 1)) / nb else out$tw
      out$boxes <- lapply(items, function(t) { pp <- parse_rich(t, TRUE)
        list(paras = pp, w = bw, h = text_height(pp, s, bw - 2 * out$inset) + 2 * out$inset) })
      hs <- vapply(out$boxes, `[[`, numeric(1), "h")
      if (out$box_row) {
        rh <- if (nb) max(hs) else 0
        out$boxes <- lapply(out$boxes, function(x) { x$h <- rh; x })   # equal heights in the row
        out$box_gap <- bgap; out$th <- rh
      } else {
        # stacked: beside the image, spread to line up with it (gaps up to 1 in)
        out$box_gap <- if (nb > 1) { if (vert) 0.2 else min(1, max(0.2, (fh - sum(hs)) / (nb - 1))) } else 0
        out$th <- if (nb) sum(hs) + out$box_gap * (nb - 1) else 0
      }
      out$paras <- list()
    } else {
      out$paras <- parse_rich(b$text, isTRUE(b$keep_lines))
      out$th <- if (length(out$paras)) text_height(out$paras, s, out$tw - 2 * out$inset) + 2 * out$inset else 0
    }
    out$h <- if (vert) fh + out$caph + (if (out$th > 0) out$th + 0.25 else 0) else max(fh + out$caph, out$th)
  } else if (b$type == "callout") {
    out$paras <- c(if (nzchar(trimws(b$title %||% ""))) style_paras(plain_paras(b$title), b = TRUE, color = NIU_RED) else list(),
                   parse_rich(b$text))
    out$inset <- 0.25; out$fixed <- FALSE
    out$h <- text_height(out$paras, s, cw - 2 * out$inset) + 2 * out$inset
  } else if (b$type == "figure") {
    a <- assets[[b$asset %||% ""]]
    asp <- if (is.null(a)) 4 / 3 else a$w / a$h
    fw <- cw * pct; fh <- fw / asp
    out$fixed <- TRUE; out$fw <- fw; out$fh <- fh; out$asset <- a
    out$cap <- caption_paras(b, b$.label); out$capsz <- csz
    out$capw <- max(fw, cw * 0.6)
    out$caph <- if (length(out$cap)) text_height(out$cap, csz, out$capw) + 0.12 else 0
    out$h <- fh + out$caph
  } else if (b$type %in% c("figrow", "figpair")) {
    # Images in a row, scaled to one common height so the row spans the full width
    ids <- unlist(b$assets) %||% character(0)
    as <- lapply(ids, function(i) assets[[i]]); as <- Filter(Negate(is.null), as)
    n <- max(length(as), b$n %||% 2)
    asps <- vapply(seq_len(n), function(k) if (k <= length(as)) as[[k]]$w / as[[k]]$h else 4 / 3, numeric(1))
    W <- cw * pct; g <- if (b$type == "figpair") 0.3 else 0.15
    h <- (W - g * (n - 1)) / sum(asps)
    out$fixed <- TRUE; out$gap <- g
    out$items <- lapply(seq_len(n), function(k) list(asset = if (k <= length(as)) as[[k]] else NULL, w = h * asps[k], h = h))
    out$fw <- W; out$fh <- h; out$labels <- isTRUE(b$labels); out$label_color <- b$label_color %||% "black"
    out$capsz <- csz
    if (b$type == "figrow") {
      out$cap <- caption_paras(b, b$.label); out$capw <- max(W, cw * 0.6)
      out$caph <- if (length(out$cap)) text_height(out$cap, csz, out$capw) + 0.12 else 0
    } else {
      # each image has its own caption (and its own figure number), sized to the image width
      caps <- unlist(b$captions) %||% character(0)
      for (k in seq_len(n)) {
        cp <- caption_paras(list(caption = if (k <= length(caps)) caps[[k]] else ""), if (length(b$.labels) >= k) b$.labels[[k]] else NULL)
        out$items[[k]]$cap <- cp
        out$items[[k]]$caph <- if (length(cp)) text_height(cp, csz, out$items[[k]]$w) + 0.12 else 0
      }
      out$cap <- list(); out$capw <- W
      out$caph <- max(0, vapply(out$items, `[[`, numeric(1), "caph"))
    }
    out$h <- h + out$caph
  } else if (b$type == "table") {
    cells <- table_cells(b, assets)
    tb <- if (is.null(cells)) table_placeholder() else prep_table(cells, b$header_mode %||% "header", b$decimals %||% 2)
    tsz <- csz; W <- cw * pct
    colw <- table_col_widths(rbind(if (!is.null(tb$header)) tb$header, tb$body), tsz, W)
    one <- function(v, w, bold = FALSE) para_lines(list(bullet = FALSE, runs = list(list(t = as.character(v), b = bold, i = FALSE))), tsz, max(w - 0.12, 0.2))
    rowh <- function(vals, bold = FALSE) max(mapply(one, vals, colw, MoreArgs = list(bold = bold))) * tsz * 1.2 / 72 + 0.1
    hs <- c(if (!is.null(tb$title)) one(tb$title, W, TRUE) * tsz * 1.2 / 72 + 0.12,
            if (!is.null(tb$header)) rowh(tb$header, TRUE) + 0.02,
            if (nrow(tb$body)) vapply(seq_len(nrow(tb$body)), function(r) rowh(tb$body[r, ]), numeric(1)))
    out$fixed <- TRUE; out$fw <- W; out$colw <- colw; out$tb <- tb; out$tsz <- tsz
    out$rowh <- hs; out$fh <- sum(hs); out$asset <- if (is.null(cells)) NULL else TRUE
    out$cap <- caption_paras(b, b$.label); out$capsz <- csz; out$capw <- max(W, cw * 0.6)
    out$caph <- if (length(out$cap)) text_height(out$cap, csz, out$capw) + 0.1 else 0
    out$h <- out$fh + out$caph
  } else if (b$type == "qr") {
    side <- cw * min(1, max(0.15, (b$size %||% 35) / 100 * scale))
    out$fixed <- TRUE; out$fw <- side; out$fh <- side; out$url <- b$url %||% ""
    out$cap <- if (nzchar(trimws(b$label %||% ""))) plain_paras(b$label) else list()
    out$capsz <- csz; out$capw <- max(side, cw * 0.6)
    out$caph <- if (length(out$cap)) text_height(out$cap, csz, out$capw) + 0.08 else 0
    out$h <- side + out$caph; out$asset <- assets[[b$asset %||% ""]]
  } else if (b$type == "equation") {
    a <- assets[[b$asset %||% ""]]
    asp <- if (is.null(a)) 5 else a$w / a$h
    # equations are sized relative to body text: height ~ 2.2 x body size at 100%
    fh <- min(s * 2.2 / 72 * (b$size %||% 100) / 100 * scale, cw / asp)
    out$fixed <- TRUE; out$fw <- fh * asp; out$fh <- fh; out$asset <- a; out$latex <- b$latex %||% ""
    out$cap <- list(); out$caph <- 0; out$h <- fh
  }
  out
}

table_col_widths <- function(m, tsz, W) {
  m <- as.matrix(m)
  if (!ncol(m)) return(W)
  need <- vapply(seq_len(ncol(m)), function(j) {
    vals <- m[, j]; vals <- vals[nzchar(vals)]; if (!length(vals)) vals <- " "
    max(vapply(vals, function(v) text_width_1line(list(list(bullet = FALSE, runs = list(list(t = v, b = TRUE, i = FALSE)))), tsz), numeric(1))) + 0.15
  }, numeric(1))
  need <- pmax(need, 0.6)
  if (sum(need) <= W) return(need * W / sum(need))
  # "water filling": narrow columns get all they need; wide (wrapping) columns share the rest
  w <- rep(NA_real_, length(need)); left <- W
  repeat {
    open <- is.na(w); fair <- left / sum(open)
    fits <- open & need <= fair
    if (!any(fits)) { w[open] <- fair; break }
    w[fits] <- need[fits]; left <- left - sum(need[fits])
    if (!any(is.na(w))) break
  }
  w * W / sum(w)
}

# ---- panel / poster layout ------------------------------------------------
heading_metrics <- function(heading, w) {
  paras <- style_paras(plain_paras(heading), b = TRUE)
  sz <- 48
  while (sz > 30 && text_width_1line(paras, sz) > w - 2 * PAD_X) sz <- sz - 2
  list(paras = paras, size = sz, h = HEAD_TOP + sz * LINE_HEIGHT / 72 + HEAD_AFTER)
}

active_blocks <- function(sec, assets, keep_empty) {
  Filter(function(b) {
    if (!block_is_empty(b, assets)) return(TRUE)
    keep_empty && b$type %in% c("figure", "figrow", "figpair", "table")
  }, sec$blocks %||% list())
}

panel_need <- function(sec, w, s, assets, keep_empty, scale = 1, grow = 1) {
  hm <- heading_metrics(sec$heading, w)
  cw <- w - 2 * PAD_X
  bl <- active_blocks(sec, assets, keep_empty)
  ms <- lapply(bl, measure_block, s = s, cw = cw, assets = assets, scale = scale, grow = grow)
  h <- hm$h + sum(vapply(ms, `[[`, numeric(1), "h")) + BLOCK_GAP * max(0, length(ms) - 1) + PAD_BOTTOM
  list(h = h, measures = ms, head = hm, cw = cw)
}

# Number figures and tables in reading order (column by column, top to bottom)
number_blocks <- function(proj, layout) {
  fig <- 0; tab <- 0
  for (col in layout$cols) for (slot in col$slots) for (k in slot) {
    sec <- proj$sections[[k]]
    if (!isTRUE(sec$visible)) next
    for (j in seq_along(sec$blocks)) {
      b <- sec$blocks[[j]]
      if (isTRUE(b$nonumber)) {
        # logos / decorative images: no "Figure N." label
      } else if (b$type == "figpair" && isTRUE(proj$settings$number %||% TRUE)) {
        # side-by-side figures: each image is its own numbered figure
        ids <- unlist(b$assets) %||% character(0)
        n <- max(length(Filter(function(a) !is.null(proj$assets[[a]]), ids)), b$n %||% 2)
        has <- vapply(seq_len(n), function(k) k <= length(ids) && !is.null(proj$assets[[ids[k]]]), logical(1))
        b$.labels <- lapply(seq_len(n), function(k) if (has[k] || isTRUE(proj$settings$keep_empty)) { fig <<- fig + 1; sprintf("Figure %d.", fig) } else NULL)
      } else if (b$type %in% c("figure", "figrow", "figpair", "figtext") && isTRUE(proj$settings$number %||% TRUE)) {
        if (!block_is_empty(b, proj$assets) || isTRUE(proj$settings$keep_empty)) { fig <- fig + 1; b$.label <- sprintf("Figure %d.", fig) }
      } else if (b$type == "table" && isTRUE(proj$settings$number %||% TRUE)) {
        if (!block_is_empty(b, proj$assets) || isTRUE(proj$settings$keep_empty)) { tab <- tab + 1; b$.label <- sprintf("Table %d.", tab) }
      }
      proj$sections[[k]]$blocks[[j]] <- b
    }
  }
  proj
}

# Insert custom boxes into the chosen layout. Each custom section has
# place = "below:<key>" or "above:<key>" relative to a built-in section; in a
# side-by-side row it joins the row, otherwise it gets its own slot in that column.
effective_layout <- function(proj) {
  L <- LAYOUTS[[proj$settings$layout %||% "classic"]]
  conts <- grep("__cont$", names(proj$sections), value = TRUE)
  for (ck in setdiff(names(proj$sections), c(SECTION_KEYS, conts))) {
    pl <- strsplit(proj$sections[[ck]]$place %||% "below:conclusion", ":", fixed = TRUE)[[1]]
    dir <- pl[1]; anchor <- pl[2]
    placed <- FALSE
    for (i in seq_along(L$cols)) {
      col <- L$cols[[i]]
      j <- which(vapply(col$slots, function(sl) anchor %in% sl, logical(1)))
      if (!length(j)) next
      sl <- col$slots[[j]]
      if (length(sl) > 1) {
        pos <- match(anchor, sl); sl <- append(sl, ck, after = if (dir == "above") pos - 1 else pos)
        col$slots[[j]] <- sl
      } else {
        at <- if (dir == "above") j - 1 else j
        col$slots <- append(col$slots, list(ck), after = at)
        col$frac <- append(col$frac, mean(col$frac) * 0.6, after = at)
      }
      L$cols[[i]] <- col; placed <- TRUE; break
    }
    if (!placed) { n <- length(L$cols); L$cols[[n]]$slots <- c(L$cols[[n]]$slots, list(ck)); L$cols[[n]]$frac <- c(L$cols[[n]]$frac, mean(L$cols[[n]]$frac) * 0.6) }
  }
  # "(cont.)" panels go at the top of the next column (or right below the
  # original when it is already in the last column)
  for (ck in conts) {
    base <- sub("__cont$", "", ck)
    i <- which(vapply(L$cols, function(col) any(vapply(col$slots, function(sl) base %in% sl, logical(1))), logical(1)))
    if (!length(i)) next
    i <- i[1]
    if (i < length(L$cols)) {
      L$cols[[i + 1]]$slots <- c(list(ck), L$cols[[i + 1]]$slots)
      L$cols[[i + 1]]$frac <- c(mean(L$cols[[i + 1]]$frac) * 0.6, L$cols[[i + 1]]$frac)
    } else {
      j <- which(vapply(L$cols[[i]]$slots, function(sl) base %in% sl, logical(1)))
      L$cols[[i]]$slots <- append(L$cols[[i]]$slots, list(ck), after = j)
      L$cols[[i]]$frac <- append(L$cols[[i]]$frac, mean(L$cols[[i]]$frac) * 0.6, after = j)
    }
  }
  L
}

# A section with `split_at` (a block id) continues in the next column from that
# block onward, as a separate "<Heading> (cont.)" panel.
expand_splits <- function(proj) {
  for (k in names(proj$sections)) {
    sec <- proj$sections[[k]]; sp <- sec$split_at %||% ""
    if (!nzchar(sp) || grepl("__cont$", k)) next
    ids <- vapply(sec$blocks, `[[`, "", "id"); j <- match(sp, ids)
    if (is.na(j) || j < 2) next
    proj$sections[[paste0(k, "__cont")]] <- list(heading = paste0(sec$heading, " (cont.)"), visible = sec$visible,
                                                 blocks = sec$blocks[j:length(ids)], cont_of = k)
    proj$sections[[k]]$blocks <- sec$blocks[seq_len(j - 1)]
  }
  proj
}

# Main entry: returns list(panels = ..., header = ..., body, warnings, ...)
compute_layout <- function(proj) {
  st <- proj$settings
  proj <- expand_splits(proj)
  layout <- effective_layout(proj)
  proj <- number_blocks(proj, layout)
  keep_empty <- isTRUE(st$keep_empty)
  smax <- max(st$body_max %||% 28, MIN_BODY); smin <- max(min(st$body_min %||% 24, smax), MIN_BODY)
  balance <- isTRUE(st$balance %||% TRUE)
  autofit <- isTRUE(st$autofit %||% TRUE) && balance
  vis <- function(k) isTRUE(proj$sections[[k]]$visible)
  grow_on <- autofit && !identical(st$grow, FALSE)
  used_scale <- 1

  # Build column plans: list of slots with visible keys
  plan_cols <- lapply(layout$cols, function(col) {
    keep <- vapply(col$slots, function(sl) any(vapply(sl, vis, logical(1))), logical(1))
    list(x = col$x, w = col$w, slots = lapply(col$slots[keep], function(sl) sl[vapply(sl, vis, logical(1))]),
         frac = col$frac[keep])
  })
  # gaps between columns are kept; widths may change when auto-adjusting
  w0 <- vapply(plan_cols, `[[`, numeric(1), "w")
  x_first <- plan_cols[[1]]$x
  gaps <- if (length(plan_cols) > 1) vapply(seq_len(length(plan_cols) - 1), function(i)
    plan_cols[[i + 1]]$x - (plan_cols[[i]]$x + plan_cols[[i]]$w), numeric(1)) else numeric(0)
  col_x <- function(w) x_first + c(0, cumsum(w + c(gaps, 0)))[seq_along(w)]

  # Height a column needs at text size s and width w
  col_needs <- function(col, w, s, scale = 1) {
    lapply(col$slots, function(sl) {
      n <- length(sl); pw <- (w - PANEL_GAP * (n - 1)) / n
      pn <- lapply(sl, function(k) panel_need(proj$sections[[k]], pw, s, proj$assets, keep_empty, scale))
      list(keys = sl, pw = pw, pn = pn, h = max(vapply(pn, `[[`, numeric(1), "h")))
    })
  }
  col_htot <- function(col) CONTENT_BOTTOM - CONTENT_TOP - PANEL_GAP * (length(col$slots) - 1)
  col_fits <- function(i, w, s) {
    col <- plan_cols[[i]]; if (!length(col$slots)) return(TRUE)
    sum(vapply(col_needs(col, w, s), `[[`, numeric(1), "h")) <= col_htot(col) + 1e-6
  }

  try_size <- function(s, scale = 1, widths = w0, route = FALSE) {
    res <- list(); ok <- TRUE; xs <- col_x(widths)
    for (i in seq_along(plan_cols)) {
      col <- plan_cols[[i]]
      ns <- length(col$slots); if (!ns) next
      Htot <- col_htot(col)
      needs <- col_needs(col, widths[i], s, scale)
      nh <- vapply(needs, `[[`, numeric(1), "h")
      if (balance) {
        # spare height is shared equally (empty panels get a double share so their
        # space stays reserved), rather than in proportion to content, which used to
        # leave the fullest panel with the biggest blank area
        if (sum(nh) <= Htot) {
          L <- Htot - sum(nh)
          empty <- vapply(needs, function(nd) all(vapply(nd$pn, function(x) !length(x$measures), logical(1))), logical(1))
          wts <- ifelse(empty, 2, 1)
          alloc <- rep(0, length(nh))
          if (route && grow_on) {
            # how much taller each slot could get if its figures grew to full width
            cap <- vapply(seq_along(needs), function(k) {
              nd <- needs[[k]]
              hmax <- max(vapply(seq_along(nd$keys), function(j)
                panel_need(proj$sections[[nd$keys[j]]], nd$pw, s, proj$assets, keep_empty, scale, 6)$h, numeric(1)))
              max(0, hmax - nh[k])
            }, numeric(1))
            if (sum(cap) > 0) alloc <- pmin(cap, L * cap / sum(cap))
          }
          heights <- nh + alloc + (L - sum(alloc)) * wts / sum(wts)
        } else heights <- Htot * nh / sum(nh)
        if (sum(nh) > Htot + 1e-6) ok <- FALSE
      } else {
        heights <- Htot * col$frac / sum(col$frac)
        if (any(nh > heights + 1e-6)) ok <- FALSE
      }
      y <- CONTENT_TOP
      for (k in seq_along(needs)) {
        nd <- needs[[k]]
        for (j in seq_along(nd$keys)) {
          res[[nd$keys[j]]] <- list(key = nd$keys[j], x = xs[i] + (j - 1) * (nd$pw + PANEL_GAP), y = y,
                                    w = nd$pw, h = heights[k], need = nd$pn[[j]])
        }
        y <- y + heights[k] + PANEL_GAP
      }
    }
    list(ok = ok, panels = res)
  }

  # Auto-adjust: for a text size s, find the narrowest width each column needs
  # (columns are independent), then check they fit side by side. Binary search s.
  total_w <- sum(w0)
  lo_w <- pmax(0.75 * w0, 5.5); hi_w <- 1.35 * w0   # stay close to the template proportions
  # smallest width (scanning lo -> hi, then refining) at which column i fits
  min_widths <- function(s) {
    vapply(seq_along(plan_cols), function(i) {
      grid <- sort(unique(c(seq(lo_w[i], hi_w[i], length.out = 9), w0[i])))
      ok <- vapply(grid, function(w) col_fits(i, w, s), logical(1))
      if (!any(ok)) return(Inf)
      k <- which(ok)[1]
      if (k == 1) return(grid[1])
      a <- grid[k - 1]; b <- grid[k]
      while (b - a > 0.1) { m <- (a + b) / 2; if (col_fits(i, m, s)) b <- m else a <- m }
      b
    }, numeric(1))
  }
  # Share spare width so columns end up about equally full. A column that would
  # overflow when widened (tall figures grow with width) is pinned at its minimum.
  auto_widths <- function(s) {
    mw <- min_widths(s)
    if (any(!is.finite(mw)) || sum(mw) > total_w + 1e-6) return(NULL)
    free <- rep(TRUE, length(mw)); w <- mw
    for (iter in 1:4) {
      slack <- total_w - sum(w)
      if (slack < 0.01) break
      room <- ifelse(free, hi_w - w, 0)
      if (sum(room) <= 0) return(NULL)
      share <- ifelse(free, mw, 0); share <- share / sum(share)
      add <- pmin(room, slack * share)
      if (sum(add) < slack - 1e-6) add <- add + (slack - sum(add)) * (room - add) / max(sum(room - add), 1e-9)
      w_try <- w + pmin(add, room)
      fits <- vapply(seq_along(w_try), function(i) !free[i] || col_fits(i, w_try[i], s), logical(1))
      if (all(fits)) { w <- w_try; break }
      free[!fits] <- FALSE; w[!fits] <- mw[!fits]
      w[fits] <- w_try[fits]
    }
    if (abs(sum(w) - total_w) > 0.05) return(NULL)
    w
  }

  # Local search: move width between columns so the emptiest column gets as full
  # as possible (figure-heavy columns fill up when widened, text columns when narrowed).
  # How full column i would be at width w: with figure growth on, figures count at
  # the size they could grow to (capped at a full column), so widening a
  # figure-heavy column is recognised as a way to fill it.
  col_fill <- function(i, w, s) {
    col <- plan_cols[[i]]; if (!length(col$slots)) return(1)
    g <- if (grow_on) 6 else 1
    h <- sum(vapply(col$slots, function(sl) {
      n <- length(sl); pw <- (w - PANEL_GAP * (n - 1)) / n
      max(vapply(sl, function(k) panel_need(proj$sections[[k]], pw, s, proj$assets, keep_empty, 1, g)$h, numeric(1)))
    }, numeric(1)))
    min(1, h / col_htot(col))
  }
  refine_widths <- function(w, s) {
    n <- length(w); if (n < 2) return(w)
    # start from the template widths when they already work at this size
    if (all(vapply(seq_len(n), function(i) col_fits(i, w0[i], s), logical(1)))) w <- w0
    f <- vapply(seq_len(n), function(i) col_fill(i, w[i], s), numeric(1))
    # reward the emptiest column getting fuller; small penalty for drifting from the template
    score <- function(f, w) min(f) + 0.01 * mean(f) - 0.012 * sum(abs(w - w0))
    best <- score(f, w)
    for (step in c(1, 0.5, 0.25)) {
      for (iter in 1:12) {
        improved <- FALSE
        for (i in seq_len(n)) for (j in seq_len(n)) if (i != j) {
          wi <- w[i] + step; wj <- w[j] - step
          if (wi > hi_w[i] || wj < lo_w[j]) next
          f2 <- f; f2[i] <- col_fill(i, wi, s); f2[j] <- col_fill(j, wj, s)
          w2 <- w; w2[i] <- wi; w2[j] <- wj
          if (!col_fits(i, wi, s) || !col_fits(j, wj, s)) next      # must still fit at the chosen text size
          sc <- score(f2, w2)
          if (sc > best + 1e-4) { w[i] <- wi; w[j] <- wj; f <- f2; best <- sc; improved <- TRUE }
        }
        if (!improved) break
      }
    }
    w
  }

  warnings <- character(0)
  chosen <- NULL; widths <- w0
  if (autofit) {
    fits_at <- function(w, s) all(vapply(seq_along(plan_cols), function(i) col_fits(i, w[i], s), logical(1)))
    # best size with the template widths
    s_t <- NA; for (s in seq(smax, smin, by = -1)) if (fits_at(w0, s)) { s_t <- s; break }
    # best size when every column may take the width it needs
    feas <- function(s) !is.null(auto_widths(s))
    s_a <- NA
    if (feas(smin)) {
      lo <- smin; hi <- smax
      if (feas(hi)) lo <- hi else while (hi - lo > 1) { m <- (lo + hi) %/% 2; if (feas(m)) lo <- m else hi <- m }
      s_a <- lo
    }
    if (!is.na(s_a) && (is.na(s_t) || s_a > s_t)) { start <- auto_widths(s_a); s_best <- s_a }
    else if (!is.na(s_t)) { start <- w0; s_best <- s_t } else s_best <- NA
    if (!is.na(s_best)) {
      widths <- refine_widths(start, s_best); body <- s_best
      chosen <- try_size(s_best, 1, widths)
      if (!chosen$ok) chosen <- NULL   # numerical edge case: fall back below
    }
  }
  if (is.null(chosen)) {
    widths <- w0
    for (s in seq(smax, smin, by = -1)) { r <- try_size(s); if (r$ok) { chosen <- r; body <- s; break } }
  }
  if (is.null(chosen)) {
    body <- smin
    # squeeze figures/tables progressively, then allow text down to the 20 pt floor (never smaller)
    for (sc in c(0.9, 0.8, 0.7, 0.6, 0.5)) { r <- try_size(body, sc); if (r$ok) { chosen <- r; used_scale <- sc; break } }
    if (!is.null(chosen)) warnings <- c(warnings, sprintf("Figures and tables were shrunk to %d%% of their set size so everything fits. Consider cutting text.", round(sc * 100)))
    if (is.null(chosen) && smin > MIN_BODY) {
      for (s2 in seq(smin - 1, MIN_BODY, by = -1)) { r <- try_size(s2, 0.5); if (r$ok) { chosen <- r; body <- s2; used_scale <- 0.5; break } }
      if (!is.null(chosen)) warnings <- c(warnings, sprintf("Text had to go down to %d pt, below your chosen minimum, to fit. Accepted PURC posters use about 24 pt body text; consider trimming.", body))
    }
    if (is.null(chosen)) {
      chosen <- try_size(MIN_BODY, 0.5); body <- MIN_BODY; used_scale <- 0.5
      warnings <- c(warnings, sprintf("Too much content to fit at a readable size (%d pt minimum). Some text runs past its panel. Please shorten text or remove a figure.", MIN_BODY))
    }
  }

  if (grow_on && balance) { rr <- try_size(body, used_scale, widths, route = TRUE); if (rr$ok || !chosen$ok) chosen <- rr }

  # Place blocks within each panel. Figures grow into spare height first; only
  # what is left becomes (modest) extra space between blocks.
  grow_figs <- grow_on
  panels <- lapply(chosen$panels, function(p) {
    nd <- p$need
    # Let figures in this panel grow (up to full width) to use spare height
    sec <- proj$sections[[p$key]]
    can_grow <- any(vapply(sec$blocks, function(b) b$type %in% c("figure", "figrow", "figpair", "figtext") && !identical(b$autogrow, FALSE), logical(1)))
    if (grow_figs && p$h - nd$h > 0.2 && can_grow) {
      need_g <- function(g) panel_need(sec, p$w, body, proj$assets, keep_empty, used_scale, g)
      lo <- 1; hi <- 6
      if (need_g(hi)$h <= p$h) lo <- hi else for (it in 1:8) { m <- (lo + hi) / 2; if (need_g(m)$h <= p$h) lo <- m else hi <- m }
      if (lo > 1.01) nd <- need_g(lo)
    }
    x0 <- p$x + PAD_X; y <- p$y + nd$head$h; cw <- nd$cw
    leftover <- max(0, p$h - nd$h)
    nb <- length(nd$measures)
    extra_gap <- if (nb > 1) min(leftover / (nb - if (autofit) 1 else 0), 0.6) else 0
    items <- list()
    for (m in nd$measures) {
      m$x <- x0; m$y <- y; m$w <- cw
      items[[length(items) + 1]] <- m
      y <- y + m$h + BLOCK_GAP + extra_gap
    }
    overflow <- nd$h > p$h + 0.05
    list(key = p$key, x = p$x, y = p$y, w = p$w, h = p$h, heading = nd$head, items = items, overflow = overflow)
  })
  for (p in panels) if (p$overflow) warnings <- c(warnings, sprintf("'%s' is too full and may overflow its panel.", proj$sections[[p$key]]$heading))
  empties <- unlist(lapply(names(proj$sections), function(k) {
    sec <- proj$sections[[k]]; if (!isTRUE(sec$visible)) return(NULL)
    n <- sum(vapply(sec$blocks, function(b) (b$type %in% c("figure", "figrow", "figpair", "table") && block_is_empty(b, proj$assets)) ||
                                            (b$type == "figtext" && is.null(proj$assets[[b$asset %||% ""]])), logical(1)))
    if (n) sprintf("%s: %d empty slot%s", sec$heading, n, if (n > 1) "s" else "")
  }))
  blank <- vapply(Filter(function(q) !length(q$items), panels), function(q) proj$sections[[q$key]]$heading, "")
  if (length(blank) && length(blank) < length(panels))
    warnings <- c(warnings, paste0("Still empty: ", paste(blank, collapse = ", "),
                                   ". Add content, or untick 'Show this section on the poster' in its tab."))
  if (length(empties) && keep_empty)
    warnings <- c(warnings, paste0("Empty slots will print as grey boxes (", paste(empties, collapse = "; "), "). Fill them, delete them, or untick 'Show empty slots'."))
  if (any(vapply(panels, function(p) any(vapply(p$items, function(it) it$type == "qr", logical(1))), logical(1))) &&
      !requireNamespace("qrcode", quietly = TRUE))
    warnings <- c(warnings, "The 'qrcode' R package is not installed, so QR codes show as placeholders.")

  list(panels = panels, header = layout_header(proj), body = body, warnings = warnings, layout = layout, proj = proj,
       justify = !identical(st$justify, FALSE), widths = widths, autofit = autofit)
}

layout_header <- function(proj) {
  m <- proj$meta
  logo2 <- proj$assets[[m$logo2 %||% ""]]
  logo2_box <- NULL
  title_w <- 23.6
  if (!is.null(logo2)) {
    maxh <- NIU_LOGO$h; maxw <- 4.6
    asp <- logo2$w / logo2$h
    h <- min(maxh, maxw / asp); w <- h * asp
    logo2_box <- list(x = NIU_LOGO$x - 0.5 - w, y = NIU_LOGO$y + (NIU_LOGO$h - h) / 2, w = w, h = h, asset = logo2)
    title_w <- min(title_w, logo2_box$x - 0.4 - 0.81)
  }
  tp <- style_paras(parse_rich(m$title %||% "", keep_lines = TRUE), b = TRUE, color = NIU_RED)
  if (!length(tp)) tp <- style_paras(plain_paras("Your poster title"), b = TRUE, color = NIU_RED)
  tbox <- list(x = 0.81, y = 0.55, w = title_w, h = 3.2)
  tf <- fit_text_size(tp, tbox$w, tbox$h, 80, 48)
  ap <- c(parse_rich(m$authors %||% "", keep_lines = TRUE), affil_paras(m$affiliations %||% ""))
  abox <- list(x = 0.81, y = 3.9, w = 30.3, h = 2.25)
  af <- fit_text_size(ap, abox$w, abox$h, 36, 22)
  list(title = c(tbox, list(paras = tp, size = tf$size)), authors = c(abox, list(paras = ap, size = af$size)), logo2 = logo2_box)
}

# "1 Department of ..." -> superscript 1 + text
affil_paras <- function(txt) {
  lines <- strsplit(gsub("\r", "", txt), "\n", fixed = TRUE)[[1]]
  lines <- lines[nzchar(trimws(lines))]
  lapply(lines, function(ln) {
    m <- regmatches(ln, regexec("^\\s*([0-9]+|[*†‡])[.):]?\\s+(.*)$", ln))[[1]]
    if (length(m)) {
      rest <- parse_runs(m[3])
      list(bullet = FALSE, runs = c(list(list(t = m[2], b = FALSE, i = FALSE, sup = TRUE, sub = FALSE)), rest))
    } else list(bullet = FALSE, runs = parse_runs(ln))
  })
}

# Absolute geometry of the pieces inside composite blocks (shared by preview + PowerPoint)
block_parts <- function(m) {
  if (m$type == "figtext") {
    gap <- 0.3
    if (isTRUE(m$vert)) {
      ix <- m$x + (m$w - m$fw) / 2
      if (m$side == "above") { ty <- m$y; iy <- m$y + (if (m$th > 0) m$th + 0.25 else 0) }
      else { iy <- m$y; ty <- iy + m$fh + m$caph + 0.25 }
      tx <- m$x
    } else {
      ix <- if (m$side == "right") m$x + m$w - m$fw else m$x
      tx <- if (m$side == "right") m$x else m$x + m$fw + gap
      iy <- m$y + max(0, (m$h - m$fh - m$caph) / 2)
      ty <- m$y + max(0, (m$h - m$th) / 2)
    }
    boxes <- NULL
    if (isTRUE(m$multi)) {
      x <- tx; y <- ty
      boxes <- lapply(m$boxes, function(bx) {
        r <- list(x = x, y = y, w = bx$w %||% m$tw, h = bx$h, paras = bx$paras)
        if (isTRUE(m$box_row)) x <<- x + r$w + m$box_gap else y <<- y + bx$h + m$box_gap
        r })
    }
    return(list(img = list(x = ix, y = iy, w = m$fw, h = m$fh), cap_y = iy + m$fh + 0.12,
                text = list(x = tx, y = ty, w = m$tw, h = m$th), boxes = boxes))
  }
  if (m$type == "flow") {
    bx <- lapply(m$boxes, function(b) list(x = m$x + b$dx, y = m$y + b$dy, w = b$w, h = b$h, paras = b$paras, row = b$row))
    arrows <- list()
    if (length(bx) > 1) for (i in seq_len(length(bx) - 1)) {
      a <- bx[[i]]; b <- bx[[i + 1]]
      if (a$row == b$row) {
        L <- if (a$x < b$x) a else b
        ah <- min(0.5, a$h * 0.6)
        arrows[[length(arrows) + 1]] <- list(x = L$x + L$w + 0.08, y = a$y + (a$h - ah) / 2, w = m$ag - 0.16, h = ah,
                                             dir = if (a$x < b$x) "right" else "left")
      } else {
        cxa <- (max(a$x, b$x) + min(a$x + a$w, b$x + b$w)) / 2   # centre of the overlap between the two boxes
        arrows[[length(arrows) + 1]] <- list(x = cxa - 0.25, y = a$y + a$h + 0.06, w = 0.5, h = m$rgap - 0.12, dir = "down")
      }
    }
    return(list(boxes = bx, arrows = arrows, cap_y = m$y + m$fh + 0.12))
  }
  NULL
}

# Rectangle of the "picture" part of a block (for the resize handle), and how
# its width responds to dragging (2 = centred, 1 = anchored on one side)
block_visual_rect <- function(m) {
  cx <- m$x + m$w / 2
  switch(m$type,
    figure = , figrow = , figpair = , qr = , equation = list(x = cx - m$fw / 2, y = m$y, w = m$fw, h = m$fh, k = 2),
    table = list(x = cx - m$fw / 2, y = m$y + m$caph, w = m$fw, h = m$fh, k = 2),
    figtext = { p <- block_parts(m); c(p$img, list(k = if (isTRUE(m$vert)) 2 else if (m$side == "right") -1 else 1)) },
    NULL)
}

# Score every layout on the current content: text size first, then how evenly full
compare_layouts <- function(proj) {
  rows <- lapply(names(LAYOUTS), function(L) {
    p <- proj; p$settings$layout <- L
    lay <- compute_layout(p)
    used <- sum(vapply(lay$panels, function(q) sum(vapply(q$items, function(m) m$h, 0)) + q$heading$h, 0))
    total <- sum(vapply(lay$panels, `[[`, 0, "h"))
    data.frame(key = L, label = LAYOUTS[[L]]$label, body = lay$body,
               empty = round(100 * (1 - used / total)), warnings = length(lay$warnings))
  })
  df <- do.call(rbind, rows)
  df$score <- df$body * 10 - df$empty * 0.3 - df$warnings * 2
  df[order(-df$score), ]
}
