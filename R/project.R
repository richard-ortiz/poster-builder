# ---------------------------------------------------------------------------
# Project state: header, settings, sections (each an ordered list of blocks)
# and assets. Default blocks mirror the slots in the NIU template.
# ---------------------------------------------------------------------------

BLOCK_TYPES <- c("Text" = "text", "Subheading" = "subheading", "Figure" = "figure",
                 "Side-by-side figures (own captions)" = "figpair",
                 "Multi-panel figure (A, B\u2026 with one caption)" = "figrow",
                 "Figure + side text" = "figtext", "Flow chart (steps with arrows)" = "flow",
                 "Table (CSV / Excel)" = "table", "Key-finding callout" = "callout",
                 "QR code" = "qr", "Equation (LaTeX)" = "equation")

new_block <- function(type, ...) {
  b <- switch(type,
    text     = list(text = "", keep_lines = FALSE, boxed = "none"),
    subheading = list(text = "", style = "box", italic = TRUE),
    flow     = list(steps = "", caption = ""),
    figtext  = list(asset = NULL, size = 50, side = "left", text = "", text_boxes = "one", boxed = "dashed", caption = "", keep_lines = FALSE, nonumber = FALSE, autogrow = TRUE),
    figure   = list(asset = NULL, size = 100, caption = "", nonumber = FALSE, autogrow = TRUE),
    figrow   = list(assets = list(), n = 2, size = 100, caption = "", labels = TRUE, label_color = "black", nonumber = FALSE, autogrow = TRUE),
    figpair  = list(assets = list(), n = 2, size = 100, captions = list("", "", ""), nonumber = FALSE, autogrow = TRUE),
    table    = list(source = "file", asset = NULL, grid = list(c("", ""), c("", ""), c("", "")),
                    header_mode = "header", size = 100, caption = "", decimals = 2),
    callout  = list(title = "Key finding", text = ""),
    qr       = list(url = "", label = "Scan for the full paper", size = 35, asset = NULL),
    equation = list(latex = "", size = 100, asset = NULL))
  b <- c(list(id = new_id("b"), type = type), b)
  args <- list(...)
  for (nm in names(args)) b[nm] <- list(args[[nm]])   # plain replacement (modifyList would merge list fields)
  b
}

default_project <- function() {
  # Start empty: every panel keeps its place on the poster, but the student adds the content
  secs <- setNames(rep(list(list()), length(SECTION_KEYS)), SECTION_KEYS)
  list(
    meta = list(title = "", authors = "", affiliations = "", logo2 = NULL),
    settings = list(layout = "classic", balance = TRUE, body_max = 28, body_min = 24, keep_empty = TRUE, number = TRUE, justify = TRUE, autofit = TRUE, grow = TRUE),
    sections = setNames(lapply(SECTION_KEYS, function(k) list(heading = unname(SECTION_DEFAULT_HEADINGS[k]), visible = TRUE, blocks = secs[[k]])), SECTION_KEYS),
    assets = list())
}

# A custom box: its own heading and blocks, placed above/below a built-in section
new_custom_section <- function(heading = "New box", place = "below:conclusion") {
  list(heading = heading, visible = TRUE, custom = TRUE, place = place, blocks = list())
}
custom_keys <- function(proj) setdiff(names(proj$sections), SECTION_KEYS)
# Short guidance shown in an empty section
SECTION_TIPS <- c(
  intro = "Background and why it matters. Aim for 150\u2013250 words; a figure or diagram helps.",
  goal = "One to three sentences: the question or hypothesis you tested.",
  methods = "How you did it. Subheadings, a flow chart of the steps, and figures of the setup work well.",
  results = "Your main findings. Lead with a key-finding callout, then figures and tables with captions.",
  conclusion = "What the results mean and what comes next. Short paragraphs or bullets.",
  ack = "Funding sources, collaborators and facilities. Logos are fine here (tick 'Don't number').",
  refs = "A few key citations, one per line.")
QUICK_TYPES <- c("Text" = "text", "Figure" = "figure", "Side-by-side figures" = "figpair", "Subheading" = "subheading",
                 "Flow chart" = "flow", "Table" = "table", "Figure + text" = "figtext")

PLACE_CHOICES <- unlist(lapply(SECTION_KEYS, function(k)
  setNames(paste0(c("above:", "below:"), k), paste(c("Above", "Below"), SECTION_DEFAULT_HEADINGS[[k]]))))

find_block <- function(proj, id) {
  for (k in names(proj$sections)) { j <- which(vapply(proj$sections[[k]]$blocks, `[[`, "", "id") == id); if (length(j)) return(c(k, j)) }
  NULL
}

# ---- save / load (.zip containing project.json + asset files) -------------
save_project <- function(proj, asset_dir, zipfile) {
  tmp <- tempfile("proj"); dir.create(tmp)
  p <- proj
  p$assets <- lapply(p$assets, function(a) { a$thumb <- NULL; a$data <- NULL; a$raw <- NULL; a$cells <- NULL; a })
  jsonlite::write_json(p, file.path(tmp, "project.json"), auto_unbox = TRUE, null = "null", pretty = TRUE)
  files <- vapply(proj$assets, `[[`, "", "file")
  dir.create(file.path(tmp, "assets"))
  if (length(files)) file.copy(file.path(asset_dir, files), file.path(tmp, "assets", files))
  old <- setwd(tmp); on.exit(setwd(old))
  utils::zip(zipfile, c("project.json", list.files("assets", full.names = TRUE)), flags = "-q")
  zipfile
}

load_project <- function(zipfile, asset_dir) {
  tmp <- tempfile("load"); dir.create(tmp)
  utils::unzip(zipfile, exdir = tmp)
  pj <- file.path(tmp, "project.json")
  if (!file.exists(pj)) stop("This doesn't look like a poster project file (no project.json inside).")
  p <- jsonlite::read_json(pj, simplifyVector = FALSE)
  base <- default_project()
  p$settings <- modifyList(base$settings, p$settings %||% list())
  p$meta <- modifyList(base$meta, p$meta %||% list())
  for (k in SECTION_KEYS) if (is.null(p$sections[[k]])) p$sections[[k]] <- base$sections[[k]]
  p$sections <- p$sections[c(SECTION_KEYS, setdiff(names(p$sections), SECTION_KEYS))]
  p$assets <- p$assets %||% list()
  for (id in names(p$assets)) {
    a <- p$assets[[id]]
    src <- file.path(tmp, "assets", a$file)
    if (!file.exists(src)) { p$assets[[id]] <- NULL; next }
    file.copy(src, file.path(asset_dir, a$file), overwrite = TRUE)
    if (identical(a$kind, "table")) {
      a$cells <- load_table_cells(src)
    } else {
      a$thumb <- thumb_from_file(file.path(asset_dir, a$file), if (identical(a$kind, "qr")) 300 else 700)
    }
    p$assets[[id]] <- a
  }
  p
}
