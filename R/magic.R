# ---------------------------------------------------------------------------
# "Magic arrange": search for the arrangement with the biggest readable text and
# the least dead space. It tries every layout and, for the fullest column,
# continuing a section into the next column ("Methods (cont.)") at each block.
# Nothing is changed until the user applies the result.
# ---------------------------------------------------------------------------

layout_stats <- function(lay) {
  used <- sum(vapply(lay$panels, function(q) sum(vapply(q$items, function(m) m$h, 0)) + q$heading$h, 0))
  total <- sum(vapply(lay$panels, `[[`, 0, "h"))
  overflow <- sum(vapply(lay$panels, function(q) isTRUE(q$overflow), logical(1)))
  shrunk <- any(grepl("shrunk|below your minimum|does not fit", lay$warnings))
  # column fill, to find the bottleneck column
  xs <- round(vapply(lay$panels, `[[`, 0, "x"))
  fill <- tapply(seq_along(lay$panels), xs, function(ix)
    sum(vapply(lay$panels[ix], function(q) sum(vapply(q$items, function(m) m$h, 0)) + q$heading$h, 0)) /
      sum(vapply(lay$panels[ix], `[[`, 0, "h")))
  list(body = lay$body, empty = round(100 * (1 - used / total)), overflow = overflow, shrunk = shrunk, col_fill = fill)
}

magic_score <- function(st, n_splits, layout_changed) {
  st$body * 10 - st$empty * 0.35 - st$overflow * 25 - (if (st$shrunk) 15 else 0) -
    n_splits * 4 - (if (layout_changed) 2 else 0)
}

# Sections (with >= 2 visible blocks) sitting in the fullest column that has a column after it
split_candidates <- function(p, lay) {
  st <- layout_stats(lay)
  xs <- sort(unique(round(vapply(lay$panels, `[[`, 0, "x"))))
  order_cols <- names(sort(st$col_fill, decreasing = TRUE))
  for (cx in order_cols) {
    if (as.numeric(cx) == max(xs)) next                     # last column: nowhere to continue
    keys <- unique(vapply(Filter(function(q) round(q$x) == as.numeric(cx), lay$panels), `[[`, "", "key"))
    keys <- keys[!grepl("__cont$", keys)]
    keys <- Filter(function(k) length(active_blocks(p$sections[[k]], p$assets, isTRUE(p$settings$keep_empty))) >= 2, keys)
    if (length(keys)) return(keys)
  }
  character(0)
}

magic_arrange <- function(proj, progress = function(v, msg) NULL) {
  cur_layout <- proj$settings$layout %||% "classic"
  evaluate <- function(p, desc) {
    lay <- compute_layout(p); st <- layout_stats(lay)
    n_splits <- sum(vapply(p$sections, function(s) nzchar(s$split_at %||% ""), logical(1)))
    list(p = p, lay = lay, stats = st, desc = desc,
         score = magic_score(st, n_splits, !identical(p$settings$layout, cur_layout)))
  }
  current <- evaluate(proj, character(0))

  # 1) every layout, with any existing splits removed
  base <- proj
  for (k in names(base$sections)) base$sections[[k]]$split_at <- NULL
  progress(0.1, "Trying layouts…")
  by_layout <- lapply(names(LAYOUTS), function(L) {
    p <- base; p$settings$layout <- L
    evaluate(p, if (L != cur_layout) sprintf("Switched to the “%s” layout", LAYOUTS[[L]]$label) else character(0))
  })
  ranked <- by_layout[order(-vapply(by_layout, `[[`, 0, "score"))]

  # 2) for the three best layouts, greedily continue sections into the next column
  finals <- list()
  tried <- 0
  for (cand in ranked[1:min(3, length(ranked))]) {
    node <- cand
    for (round in 1:2) {
      keys <- split_candidates(node$p, node$lay)
      improved <- NULL
      for (k in keys) {
        ids <- vapply(active_blocks(node$p$sections[[k]], node$p$assets, isTRUE(node$p$settings$keep_empty)), `[[`, "", "id")
        for (j in seq_along(ids)[-1]) {
          tried <- tried + 1
          progress(min(0.15 + tried * 0.02, 0.95), "Trying to continue sections into the next column…")
          p <- node$p; p$sections[[k]]$split_at <- ids[j]
          e <- evaluate(p, c(node$desc, sprintf("Continued “%s” into the next column (from block %d of %d)",
                                                 p$sections[[k]]$heading, j, length(ids))))
          if (e$score > (improved$score %||% node$score)) improved <- e
        }
      }
      if (is.null(improved)) break
      node <- improved
    }
    # 3) if panels still have room, allow somewhat larger body text (max 36 pt)
    smax <- node$p$settings$body_max %||% 28
    for (m in c(smax + 4, smax + 8)) {
      if (m > 36 || node$stats$body < smax || node$stats$empty < 15) break
      p <- node$p; p$settings$body_max <- m
      e <- evaluate(p, c(node$desc[!grepl("^Raised", node$desc)], sprintf("Raised the maximum text size to %d pt to fill empty space", m)))
      if (e$score > node$score) node <- e else break
    }
    finals[[length(finals) + 1]] <- node
  }
  progress(1, "Done")
  finals <- finals[order(-vapply(finals, `[[`, 0, "score"))]
  # keep options that beat the current poster
  options <- Filter(function(o) o$score > current$score + 0.5, finals)
  list(before = current, options = options)
}
