# ---------------------------------------------------------------------------
# Live HTML preview built from the same layout the PowerPoint uses.
# Rendered at PX pixels per inch and scaled to the panel width in the browser.
# ---------------------------------------------------------------------------

PX <- 30
px <- function(inches) sprintf("%.1fpx", inches * PX)
fpx <- function(pt) pt / 72 * PX

box_style <- function(x, y, w, h = NULL) {
  paste0("position:absolute;left:", px(x), ";top:", px(y), ";width:", px(w), if (!is.null(h)) paste0(";height:", px(h)) else "", ";")
}

text_div <- function(paras, size, x, y, w, h = NULL, color = "#000", align = "left", extra = "") {
  sprintf('<div class="ptext" style="%sfont-size:%.2fpx;line-height:%.2f;color:%s;text-align:%s;%s">%s</div>',
          box_style(x, y, w, h), fpx(size), LINE_HEIGHT, color, align, extra, paras_html(paras, fpx(size)))
}

img_div <- function(a, x, y, w, h, label = "figure") {
  if (is.null(a)) {
    sprintf('<div class="pempty" style="%sfont-size:%.1fpx">%s</div>', box_style(x, y, w, h), max(8, min(40, h * PX * 0.18)), label)
  } else sprintf('<img src="%s" style="%sobject-fit:fill" />', a$thumb, box_style(x, y, w, h))
}

table_html <- function(m, x, y) {
  tb <- m$tb; n <- tb$ncol
  sz <- fpx(m$tsz)
  cols <- paste0(sprintf('<col style="width:%s">', px(m$colw)), collapse = "")
  title <- if (!is.null(tb$title)) sprintf('<thead><tr><th colspan="%d" style="text-align:center">%s</th></tr></thead>', n, html_escape(tb$title)) else ""
  head <- if (!is.null(tb$header)) paste0("<thead><tr>", paste0("<th>", html_escape(c(tb$header, rep("", n))[seq_len(n)]), "</th>", collapse = ""), "</tr></thead>") else ""
  rows <- if (nrow(tb$body)) paste0(vapply(seq_len(nrow(tb$body)), function(r)
    paste0("<tr>", paste0("<td>", html_escape(c(tb$body[r, ], rep("", n))[seq_len(n)]), "</td>", collapse = ""), "</tr>"), ""), collapse = "") else ""
  sprintf('<table class="ptable" style="%sfont-size:%.2fpx">%s%s%s<tbody>%s</tbody></table>', box_style(x, y, m$fw), sz, cols, title, head, rows)
}

arrow_svg <- function(a) {
  pts <- switch(a$dir,
    right = "0,30 60,30 60,10 100,50 60,90 60,70 0,70",
    left  = "100,30 40,30 40,10 0,50 40,90 40,70 100,70",
    down  = "30,0 70,0 70,60 90,60 50,100 10,60 30,60")
  sprintf('<svg class="parrow" viewBox="0 0 100 100" preserveAspectRatio="none" style="%s"><polygon points="%s" fill="#%s"/></svg>',
          box_style(a$x, a$y, a$w, a$h), pts, NIU_RED)
}

preview_html <- function(proj, lay) {
  proj <- lay$proj %||% proj   # includes "(cont.)" panels and figure numbers
  out <- character(0)
  body_align <- if (isTRUE(lay$justify)) "justify" else "left"
  add <- function(...) out <<- c(out, ...)
  # background + NIU logo (from the template master)
  add(sprintf('<div style="position:absolute;left:0;top:%s;width:100%%;height:%s;background:linear-gradient(#fff,#%s)"></div>', px(13.3), px(POSTER_H - 13.3), NIU_RED))
  add(sprintf('<img src="niu_logo.png" style="%s"/>', box_style(NIU_LOGO$x, NIU_LOGO$y, NIU_LOGO$w, NIU_LOGO$h)))
  h <- lay$header
  add(sprintf('<div style="%sdisplay:flex;align-items:flex-end;cursor:pointer" onclick="Shiny.setInputValue(\'preview_click\',\'header\',{priority:\'event\'})"><div class="ptext" style="font-size:%.2fpx;line-height:%.2f;color:#%s">%s</div></div>',
              box_style(h$title$x, h$title$y, h$title$w, h$title$h), fpx(h$title$size), LINE_HEIGHT, NIU_RED,
              paras_html(h$title$paras, fpx(h$title$size))))
  if (length(h$authors$paras)) add(text_div(h$authors$paras, h$authors$size, h$authors$x, h$authors$y, h$authors$w))
  else add(sprintf('<div class="phint" style="%sfont-size:%.1fpx">Authors and affiliations go here</div>', box_style(h$authors$x, h$authors$y, h$authors$w), fpx(30)))
  if (!is.null(h$logo2)) add(img_div(h$logo2$asset, h$logo2$x, h$logo2$y, h$logo2$w, h$logo2$h))

  for (p in lay$panels) {
    sec <- proj$sections[[p$key]]
    add(sprintf('<div class="ppanel%s" data-key="%s" title="Click to edit %s" onclick="Shiny.setInputValue(\'preview_click\',\'%s\',{priority:\'event\'})" style="%sborder-radius:%s"></div>',
                if (p$overflow) " poverflow" else "", p$key, html_escape(sec$heading), p$key, box_style(p$x, p$y, p$w, p$h), px(min(0.55, min(p$w, p$h) / 2))))
    add(text_div(p$heading$paras, p$heading$size, p$x + PAD_X - 0.05, p$y + HEAD_TOP, p$w - 2 * PAD_X + 0.1, color = paste0("#", NIU_RED)))
    if (!length(p$items))
      add(sprintf('<div class="pempty-panel" style="%s"><span>+ Click to add content</span></div>',
                  box_style(p$x + PAD_X, p$y + p$heading$h, p$w - 2 * PAD_X, max(0.6, p$h - p$heading$h - PAD_BOTTOM))))
    for (m in p$items) {
      cx <- m$x + m$w / 2
      cap <- function(y) if (length(m$cap)) add(text_div(m$cap, m$capsz, max(m$x, cx - m$capw / 2), y, m$capw, align = if (m$type == "qr") "center" else "left"))
      if (m$type == "text") {
        if (m$boxed != "none") add(sprintf('<div class="pframe %s" style="%s"></div>', m$boxed, box_style(m$x, m$y, m$w, m$h)))
        add(text_div(m$paras, lay$body, m$x + m$inset, m$y + m$inset, m$w - 2 * m$inset, align = body_align))
      } else if (m$type == "subheading") {
        cls <- paste0("psub psub-", m$style)
        add(sprintf('<div class="%s" style="%sfont-size:%.2fpx;padding:0 %s;">%s</div>', cls, box_style(m$x, m$y, m$w, m$h), fpx(m$size), px(m$inset),
                    paras_html(lapply(m$paras, function(p) p), fpx(m$size))))
      } else if (m$type == "flow") {
        pr <- block_parts(m)
        for (b in pr$boxes) add(sprintf('<div class="pflow" style="%sfont-size:%.2fpx;padding:%s">%s</div>', box_style(b$x, b$y, b$w, b$h), fpx(m$fsz), px(m$inset), paras_html(b$paras, fpx(m$fsz))))
        for (a in pr$arrows) add(arrow_svg(a))
        if (length(m$cap)) add(text_div(m$cap, m$capsz, m$x, pr$cap_y, m$capw))
      } else if (m$type == "figtext") {
        pr <- block_parts(m)
        add(img_div(m$asset, pr$img$x, pr$img$y, pr$img$w, pr$img$h))
        if (length(m$cap)) add(text_div(m$cap, m$capsz, pr$img$x, pr$cap_y, m$capw))
        if (length(m$paras)) {
          t <- pr$text
          if (m$boxed != "none") add(sprintf('<div class="pframe %s" style="%s"></div>', m$boxed, box_style(t$x, t$y, t$w, t$h)))
          add(text_div(m$paras, lay$body, t$x + m$inset, t$y + m$inset, t$w - 2 * m$inset, align = body_align))
        }
        for (t in pr$boxes %||% list()) {
          if (m$boxed != "none") add(sprintf('<div class="pframe %s" style="%s"></div>', m$boxed, box_style(t$x, t$y, t$w, t$h)))
          add(text_div(t$paras, lay$body, t$x + m$inset, t$y + m$inset, t$w - 2 * m$inset, t$h - 2 * m$inset, align = "left",
                       extra = "display:flex;flex-direction:column;justify-content:center;"))
        }
      }
      else if (m$type == "callout") {
        add(sprintf('<div class="pcallout" style="%s"></div>', box_style(m$x, m$y, m$w, m$h)))
        add(text_div(m$paras, lay$body, m$x + m$inset, m$y + m$inset, m$w - 2 * m$inset, align = body_align))
      } else if (m$type == "figure") {
        add(img_div(m$asset, cx - m$fw / 2, m$y, m$fw, m$fh)); cap(m$y + m$fh + 0.12)
      } else if (m$type %in% c("figrow", "figpair")) {
        x <- cx - m$fw / 2
        for (k in seq_along(m$items)) {
          it <- m$items[[k]]
          add(img_div(it$asset, x, m$y, it$w, it$h))
          if (m$type == "figrow" && m$labels)
            add(sprintf('<div style="%sfont:bold %.1fpx Arial;color:%s">%s</div>', box_style(x + 0.08, m$y + 0.04, 1), fpx(lay$body * 1.3),
                        if (identical(m$label_color, "white")) "#fff" else "#000", LETTERS[k]))
          if (m$type == "figpair" && length(it$cap)) add(text_div(it$cap, m$capsz, x, m$y + m$fh + 0.12, it$w))
          x <- x + it$w + m$gap
        }
        if (m$type == "figrow") cap(m$y + m$fh + 0.12)
      } else if (m$type == "table") {
        cap(m$y)
        if (is.null(m$asset)) add(img_div(NULL, cx - m$fw / 2, m$y + m$caph, m$fw, m$fh, "table"))
        else add(table_html(m, cx - m$fw / 2, m$y + m$caph))
      } else if (m$type == "qr") {
        add(img_div(m$asset, cx - m$fw / 2, m$y, m$fw, m$fh, "QR")); cap(m$y + m$fh + 0.08)
      } else if (m$type == "equation") {
        add(img_div(m$asset, cx - m$fw / 2, m$y, m$fw, m$fh, "equation"))
      }
    }
  }
  # drag / resize overlays (one per block) drawn on top of everything
  for (p in lay$panels) for (m in p$items) {
    add(sprintf('<div class="pitem" data-id="%s" data-key="%s" style="%s"></div>', m$id, p$key, box_style(m$x, m$y, m$w, m$h)))
    v <- block_visual_rect(m)
    if (!is.null(v)) {
      size <- (proj$sections[[p$key]]$blocks[[which(vapply(proj$sections[[p$key]]$blocks, `[[`, "", "id") == m$id)]]$size) %||% 100
      # figures may have grown: start the drag from the width actually shown, as % of the panel
      if (m$type %in% c("figure", "figrow", "figpair", "figtext", "table", "qr")) size <- round(100 * v$w / m$w)
      add(sprintf('<div class="phandle" title="Drag to resize" data-id="%s" data-k="%d" data-size="%s" data-fw="%.2f" data-fh="%.2f" data-fx="%.2f" data-fy="%.2f" style="left:%s;top:%s"></div>',
                  m$id, as.integer(v$k), size, v$w * PX, v$h * PX, v$x * PX, v$y * PX, px(if (v$k < 0) v$x + 0.05 else v$x + v$w - 0.35), px(v$y + v$h - 0.35)))
    }
  }
  sprintf('<div class="poster" style="width:%s;height:%s">%s</div>', px(POSTER_W), px(POSTER_H), paste0(out, collapse = ""))
}
