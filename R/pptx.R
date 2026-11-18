# ---------------------------------------------------------------------------
# PowerPoint output. Starts from the blank NIU template (gradient + NIU logo live
# on the slide master), then adds panels, headings and text as native shapes
# (so everything stays editable in PowerPoint), and images/tables via officer.
# ---------------------------------------------------------------------------

NS <- 'xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"'
# Parse from raw bytes so non-ASCII text (µ, ±, Greek) survives regardless of the server locale
read_xml_utf8 <- function(s) xml2::read_xml(charToRaw(enc2utf8(s)), encoding = "UTF-8")
emu <- function(inches) format(round(inches * 914400), scientific = FALSE, trim = TRUE)
xml_esc <- function(x) { x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); x <- gsub(">", "&gt;", x, fixed = TRUE); gsub("\"", "&quot;", x, fixed = TRUE) }

run_xml <- function(r, size, color = "000000") {
  col <- r$color %||% color
  attrs <- sprintf('lang="en-US" sz="%d"%s%s%s', round(size * 100),
                   if (isTRUE(r$b)) ' b="1"' else '', if (isTRUE(r$i)) ' i="1"' else '',
                   if (isTRUE(r$sup)) ' baseline="30000"' else if (isTRUE(r$sub)) ' baseline="-25000"' else '')
  sprintf('<a:r><a:rPr %s dirty="0"><a:solidFill><a:srgbClr val="%s"/></a:solidFill><a:latin typeface="Arial"/><a:cs typeface="Arial"/></a:rPr><a:t>%s</a:t></a:r>',
          attrs, col, xml_esc(r$t))
}

paras_xml <- function(paras, size, color = "000000", align = "l") {
  if (!length(paras)) return(sprintf('<a:p><a:endParaRPr lang="en-US" sz="%d"/></a:p>', round(size * 100)))
  paste0(vapply(seq_along(paras), function(k) {
    p <- paras[[k]]
    spc <- if (k < length(paras)) sprintf('<a:spcAft><a:spcPts val="%d"/></a:spcAft>', round(size * PARA_SPACE * 100)) else ''
    bu <- if (isTRUE(p$bullet)) sprintf('<a:buFont typeface="Arial"/><a:buChar char="•"/>') else '<a:buNone/>'
    ind <- if (isTRUE(p$bullet)) sprintf(' marL="%s" indent="-%s"', emu(size * 1.1 / 72), emu(size * 1.1 / 72)) else ''
    sprintf('<a:p><a:pPr algn="%s"%s><a:lnSpc><a:spcPct val="100000"/></a:lnSpc>%s%s</a:pPr>%s<a:endParaRPr lang="en-US" sz="%d"/></a:p>',
            align, ind, spc, bu, paste0(vapply(p$runs, run_xml, character(1), size = size, color = color), collapse = ""), round(size * 100))
  }, character(1)), collapse = "")
}

shape_xml <- function(name, x, y, w, h, geom = "rect", fill = NULL, line = NULL, line_w = 0,
                      paras = NULL, size = 24, color = "000000", align = "l", anchor = "t", inset = 0, adj = NULL, dash = FALSE) {
  fill_x <- if (is.null(fill)) "<a:noFill/>" else sprintf('<a:solidFill><a:srgbClr val="%s"/></a:solidFill>', fill)
  line_x <- if (is.null(line)) "<a:ln><a:noFill/></a:ln>" else sprintf('<a:ln w="%s"><a:solidFill><a:srgbClr val="%s"/></a:solidFill>%s</a:ln>',
                                                                        emu(line_w / 72), line, if (dash) '<a:prstDash val="dash"/>' else '')
  av <- if (is.null(adj)) "<a:avLst/>" else sprintf('<a:avLst><a:gd name="adj" fmla="val %d"/></a:avLst>', adj)
  tx <- if (is.null(paras)) sprintf('<p:txBody><a:bodyPr rtlCol="0" anchor="ctr"/><a:lstStyle/><a:p><a:endParaRPr lang="en-US"/></a:p></p:txBody>') else
    sprintf('<p:txBody><a:bodyPr wrap="square" lIns="%s" tIns="%s" rIns="%s" bIns="%s" rtlCol="0" anchor="%s"><a:noAutofit/></a:bodyPr><a:lstStyle/>%s</p:txBody>',
            emu(inset), emu(inset), emu(inset), emu(inset), anchor, paras_xml(paras, size, color, align))
  sprintf('<p:sp %s><p:nvSpPr><p:cNvPr id="%d" name="%s"/><p:cNvSpPr%s/><p:nvPr/></p:nvSpPr><p:spPr><a:xfrm><a:off x="%s" y="%s"/><a:ext cx="%s" cy="%s"/></a:xfrm><a:prstGeom prst="%s">%s</a:prstGeom>%s%s</p:spPr>%s</p:sp>',
          NS, sample(10000:99999, 1), xml_esc(name), if (!is.null(paras) && is.null(fill)) ' txBox="1"' else '',
          emu(x), emu(y), emu(w), emu(h), geom, av, fill_x, line_x, tx)
}

frame_xml <- function(x, y, w, h, style) {
  shape_xml("Text frame", x, y, w, h, geom = "roundRect", fill = "FFFFFF", line = NIU_RED, line_w = 2.5,
            dash = identical(style, "dashed"), adj = round(0.2 / min(w, h) * 1e5))
}

placeholder_xml <- function(x, y, w, h, label = "figure") {
  shape_xml("Empty slot", x, y, w, h, fill = "D9D9D9",
            paras = plain_paras(label), size = max(14, min(60, h * 72 * 0.18)), color = "FFFFFF", align = "ctr", anchor = "ctr")
}

build_flextable <- function(m) {
  tb <- m$tb; n <- tb$ncol
  body <- tb$body
  if (!nrow(body)) body <- matrix("", 1, n)
  if (ncol(body) < n) body <- cbind(body, matrix("", nrow(body), n - ncol(body)))
  df <- as.data.frame(body, stringsAsFactors = FALSE); names(df) <- paste0("V", seq_len(n))
  ft <- flextable::flextable(df)
  has_head <- !is.null(tb$header) || !is.null(tb$title)
  if (!is.null(tb$header)) {
    ft <- flextable::set_header_labels(ft, values = setNames(as.list(c(tb$header, rep("", n))[seq_len(n)]), names(df)))
  } else if (!is.null(tb$title)) {
    ft <- flextable::set_header_labels(ft, values = setNames(as.list(c(tb$title, rep("", n - 1))), names(df)))
    if (n > 1) ft <- flextable::merge_at(ft, i = 1, j = seq_len(n), part = "header")
    ft <- flextable::align(ft, align = "center", part = "header")
  } else ft <- flextable::delete_part(ft, part = "header")
  ft <- flextable::font(ft, fontname = "Arial", part = "all")
  ft <- flextable::fontsize(ft, size = m$tsz, part = "all")
  if (has_head) {
    ft <- flextable::bold(ft, part = "header")
    ft <- flextable::bg(ft, bg = paste0("#", NIU_RED), part = "header")
    ft <- flextable::color(ft, color = "white", part = "header")
  }
  if (nrow(df) > 1) ft <- flextable::bg(ft, i = seq(2, nrow(df), by = 2), bg = "#F4F4F4", part = "body")
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_bottom(ft, border = officer::fp_border(color = paste0("#", NIU_RED), width = 2), part = "body")
  ft <- flextable::padding(ft, padding.top = 2, padding.bottom = 2, padding.left = 4, padding.right = 4, part = "all")
  ft <- flextable::width(ft, width = m$colw)
  ft <- flextable::valign(ft, valign = "center", part = "all")
  ft
}

build_pptx <- function(proj, lay, asset_dir, template, out_file) {
  proj <- lay$proj %||% proj   # includes "(cont.)" panels and figure numbers
  doc <- officer::read_pptx(template)
  doc <- officer::on_slide(doc, 1)
  back <- character(0); front <- character(0)
  keep_empty <- isTRUE(proj$settings$keep_empty)
  body_align <- if (isTRUE(lay$justify)) "just" else "l"

  add_img <- function(a, x, y, w, h) {
    doc <<- officer::ph_with(doc, officer::external_img(asset_path(a, asset_dir), width = w, height = h),
                             location = officer::ph_location(left = x, top = y, width = w, height = h))
  }
  add_caption <- function(m, x_center, y) {
    if (!length(m$cap)) return()
    cx <- max(m$x, x_center - m$capw / 2)
    front <<- c(front, shape_xml("Caption", cx, y, m$capw, text_height(m$cap, m$capsz, m$capw) + 0.05,
                                 paras = m$cap, size = m$capsz))
  }

  # header
  h <- lay$header
  front <- c(front, shape_xml("Title", h$title$x, h$title$y, h$title$w, h$title$h, paras = h$title$paras, size = h$title$size, color = NIU_RED, anchor = "b"))
  if (length(h$authors$paras))
    front <- c(front, shape_xml("Authors", h$authors$x, h$authors$y, h$authors$w, h$authors$h, paras = h$authors$paras, size = h$authors$size))
  if (!is.null(h$logo2)) add_img(h$logo2$asset, h$logo2$x, h$logo2$y, h$logo2$w, h$logo2$h)

  for (p in lay$panels) {
    sec <- proj$sections[[p$key]]
    back <- c(back, shape_xml(paste("Panel", sec$heading), p$x, p$y, p$w, p$h, geom = "roundRect", fill = "FFFFFF", adj = round(0.55 / min(p$w, p$h) * 1e5)))
    hm <- p$heading
    front <- c(front, shape_xml(paste("Heading", sec$heading), p$x + PAD_X - 0.05, p$y + HEAD_TOP, p$w - 2 * PAD_X + 0.1, hm$size * LINE_HEIGHT / 72 + 0.1,
                                paras = hm$paras, size = hm$size, color = NIU_RED))
    for (m in p$items) {
      cx <- m$x + m$w / 2
      if (m$type == "text") {
        if (m$boxed != "none") back <- c(back, frame_xml(m$x, m$y, m$w, m$h, m$boxed))
        front <- c(front, shape_xml("Text", m$x + m$inset, m$y + m$inset, m$w - 2 * m$inset, m$h - 2 * m$inset + 0.1, paras = m$paras, size = lay$body, align = body_align))
      } else if (m$type == "subheading") {
        st <- m$style
        front <- c(front, shape_xml("Subheading", m$x, m$y, m$w, m$h, geom = if (st == "plain") "rect" else "roundRect",
                                    fill = if (st == "filled") NIU_RED else if (st == "box") "FFFFFF" else NULL,
                                    line = if (st == "box") NIU_RED else NULL, line_w = 3, adj = round(0.15 / min(m$w, m$h) * 1e5),
                                    paras = m$paras, size = m$size, color = if (st == "filled") "FFFFFF" else if (st == "plain") NIU_RED else "000000",
                                    align = "ctr", anchor = "ctr", inset = m$inset))
        if (st == "plain") front <- c(front, shape_xml("Subheading rule", m$x, m$y + m$h - 0.02, m$w, 0.04, fill = NIU_RED))
      } else if (m$type == "flow") {
        pr <- block_parts(m)
        for (b in pr$boxes) front <- c(front, shape_xml("Flow step", b$x, b$y, b$w, b$h, geom = "roundRect", fill = "FFFFFF", line = NIU_RED, line_w = 3,
                                                        adj = round(0.2 / min(b$w, b$h) * 1e5), paras = b$paras, size = m$fsz,
                                                        align = "ctr", anchor = "ctr", inset = m$inset))
        for (a in pr$arrows) front <- c(front, shape_xml("Flow arrow", a$x, a$y, a$w, a$h, geom = paste0(a$dir, "Arrow"), fill = NIU_RED))
        if (length(m$cap)) front <- c(front, shape_xml("Caption", m$x, pr$cap_y, m$capw, m$caph, paras = m$cap, size = m$capsz))
      } else if (m$type == "figtext") {
        pr <- block_parts(m)
        if (is.null(m$asset)) front <- c(front, placeholder_xml(pr$img$x, pr$img$y, pr$img$w, pr$img$h)) else add_img(m$asset, pr$img$x, pr$img$y, pr$img$w, pr$img$h)
        if (length(m$cap)) front <- c(front, shape_xml("Caption", pr$img$x, pr$cap_y, m$capw, m$caph, paras = m$cap, size = m$capsz))
        if (length(m$paras)) {
          t <- pr$text
          if (m$boxed != "none") back <- c(back, frame_xml(t$x, t$y, t$w, t$h, m$boxed))
          front <- c(front, shape_xml("Side text", t$x + m$inset, t$y + m$inset, t$w - 2 * m$inset, t$h - 2 * m$inset + 0.1, paras = m$paras, size = lay$body, align = body_align))
        }
        for (t in pr$boxes %||% list()) {
          if (m$boxed != "none") back <- c(back, frame_xml(t$x, t$y, t$w, t$h, m$boxed))
          front <- c(front, shape_xml("Side text box", t$x + m$inset, t$y + m$inset, t$w - 2 * m$inset, t$h - 2 * m$inset + 0.1,
                                      paras = t$paras, size = lay$body, align = "l", anchor = "ctr"))
        }
      } else if (m$type == "callout") {
        back <- c(back, shape_xml("Callout box", m$x, m$y, m$w, m$h, geom = "roundRect", fill = "FBE7EA", line = NIU_RED, line_w = 3, adj = round(0.2 / min(m$w, m$h) * 1e5)))
        front <- c(front, shape_xml("Callout text", m$x + m$inset, m$y + m$inset, m$w - 2 * m$inset, m$h - 2 * m$inset + 0.05, paras = m$paras, size = lay$body, align = body_align))
      } else if (m$type == "figure") {
        fx <- cx - m$fw / 2
        if (is.null(m$asset)) front <- c(front, placeholder_xml(fx, m$y, m$fw, m$fh)) else add_img(m$asset, fx, m$y, m$fw, m$fh)
        add_caption(m, cx, m$y + m$fh + 0.12)
      } else if (m$type %in% c("figrow", "figpair")) {
        x <- cx - m$fw / 2
        for (k in seq_along(m$items)) {
          it <- m$items[[k]]
          if (is.null(it$asset)) front <- c(front, placeholder_xml(x, m$y, it$w, it$h)) else add_img(it$asset, x, m$y, it$w, it$h)
          if (m$type == "figrow" && m$labels)
            front <- c(front, shape_xml("Panel label", x + 0.08, m$y + 0.04, 1, lay$body * 1.6 / 72,
                                        paras = plain_paras(LETTERS[k], b = TRUE), size = round(lay$body * 1.3),
                                        color = if (identical(m$label_color, "white")) "FFFFFF" else "000000"))
          if (m$type == "figpair" && length(it$cap))
            front <- c(front, shape_xml("Caption", x, m$y + m$fh + 0.12, it$w, it$caph, paras = it$cap, size = m$capsz))
          x <- x + it$w + m$gap
        }
        if (m$type == "figrow") add_caption(m, cx, m$y + m$fh + 0.12)
      } else if (m$type == "table") {
        add_caption(m, cx, m$y)
        ty <- m$y + m$caph
        if (is.null(m$asset)) front <- c(front, placeholder_xml(cx - m$fw / 2, ty, m$fw, m$fh, "table"))
        else doc <- officer::ph_with(doc, build_flextable(m), location = officer::ph_location(left = cx - m$fw / 2, top = ty))
      } else if (m$type == "qr") {
        if (!is.null(m$asset)) add_img(m$asset, cx - m$fw / 2, m$y, m$fw, m$fh)
        if (length(m$cap)) front <- c(front, shape_xml("QR label", cx - m$capw / 2, m$y + m$fh + 0.08, m$capw, m$caph + 0.05,
                                                     paras = m$cap, size = m$capsz, align = "ctr"))
      } else if (m$type == "equation") {
        if (!is.null(m$asset)) add_img(m$asset, cx - m$fw / 2, m$y, m$fw, m$fh)
      }
    }
  }

  sld <- doc$slide$get_slide(1)$get()
  tree <- xml2::xml_find_first(sld, "//p:spTree")
  grp <- xml2::xml_find_first(tree, "p:grpSpPr")
  for (s in rev(back)) xml2::xml_add_sibling(grp, read_xml_utf8(s), .where = "after")
  for (s in front) xml2::xml_add_child(tree, read_xml_utf8(s))
  # officer tags inserted pictures as layout placeholders; make them ordinary pictures
  xml2::xml_remove(xml2::xml_find_all(tree, ".//p:pic/p:nvPicPr/p:nvPr/p:ph"))
  pics <- xml2::xml_find_all(tree, ".//p:pic/p:nvPicPr/p:cNvPr")
  for (k in seq_along(pics)) xml2::xml_set_attr(pics[[k]], "name", paste("Picture", k))
  frames <- xml2::xml_find_all(tree, ".//p:graphicFrame/p:nvGraphicFramePr/p:cNvPr")
  for (k in seq_along(frames)) xml2::xml_set_attr(frames[[k]], "name", paste("Table", k))
  print(doc, target = out_file)
  invisible(out_file)
}
