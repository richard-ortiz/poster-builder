# ===========================================================================
#  NIU Poster Builder
#  Build a 40 x 32 in. conference poster in the NIU template style.
#  Fill in sections, drop in figures/tables, resize them, preview live, and
#  download an editable PowerPoint file.
#  Files in R/ are loaded automatically by Shiny.
# ===========================================================================

library(shiny)
library(bslib)

options(shiny.maxRequestSize = 150 * 1024^2)
invisible(tryCatch(Sys.setlocale("LC_CTYPE", "C.UTF-8"), warning = function(w) NULL, error = function(e) NULL))
TEMPLATE <- "template/poster_40x32_base.pptx"

FORMAT_HELP <- tags$details(class = "fmt-tips", tags$summary(icon("circle-question"), " Formatting tips"),
  HTML("<code>**bold**</code> &nbsp; <code>*italic*</code> &nbsp; <code>^superscript^</code> &nbsp; <code>~subscript~</code> &nbsp; start a line with <code>- </code> for a bullet. Leave a blank line between paragraphs. Text resizes itself to fit."))

block_label <- function(type) names(BLOCK_TYPES)[BLOCK_TYPES == type]

section_tab <- function(k, heading = SECTION_DEFAULT_HEADINGS[[k]], custom = FALSE, place = NULL) {
  tabPanel(if (custom) tagList(icon("square-plus"), heading) else heading, value = k,
    div(class = "sec-head",
      textInput(paste0("heading_", k), "Panel heading", heading, width = "100%"),
      checkboxInput(paste0("visible_", k), "Show this section on the poster", TRUE)),
    if (custom) div(class = "sec-head custom-place",
      selectInput(paste0("place_", k), "Position on the poster", PLACE_CHOICES, selected = place, width = "100%", selectize = FALSE),
      actionButton(paste0("delbox_", k), "Delete this box", icon = icon("trash"), class = "btn-outline-danger btn-sm")),
    uiOutput(paste0("blocks_", k)),
    div(class = "add-row",
      span(class = "add-label", "Add:"),
      { qt <- quick_types_for(k)
        lapply(names(qt), function(nm) tags$button(type = "button", class = "btn btn-outline-primary btn-sm quick-add",
          onclick = sprintf("Shiny.setInputValue('quick_add', {key:'%s', type:'%s', t:Date.now()}, {priority:'event'})", k, qt[[nm]]),
          icon("plus"), nm)) },
      tags$details(class = "more-types", tags$summary("More\u2026"),
        div(class = "add-more",
          selectInput(paste0("addtype_", k), NULL, BLOCK_TYPES, selected = "callout", width = "230px", selectize = FALSE),
          actionButton(paste0("add_", k), "Add", icon = icon("plus"), class = "btn-primary btn-sm")))),
    FORMAT_HELP)
}

ui <- page_fluid(
  theme = bs_theme(version = 5, primary = "#C8102E", base_font = font_collection("Arial", "Helvetica", "sans-serif"), "font-size-base" = "0.92rem"),
  tags$head(tags$link(rel = "stylesheet", href = "app.css"), tags$script(src = "app.js")),
  div(class = "app-header",
      tags$img(src = "niu_logo.png", height = "38px"),
      div(class = "app-titles", h3("Poster Builder"),
          div(class = "app-sub", "Psychology Undergraduate Research Conference \u00b7 40 \u00d7 32 in. poster"),
          div(class = "app-lab", "Developed by the Ortiz Neuroscience Laboratory, Department of Psychology, Northern Illinois University")),
      div(class = "app-about", actionLink("about", tagList(icon("circle-info"), " About & disclaimer")))),
  uiOutput("pkg_banner"),
  uiOutput("welcome"),
  fluidRow(
    column(5, class = "editor-col",
      do.call(tabsetPanel, c(list(id = "tabs", type = "pills",
        tabPanel("Header", value = "header",
          textAreaInput("title", "Poster title", "", rows = 2, width = "100%", placeholder = "A clear, specific title (one or two lines)"),
          textAreaInput("authors", "Authors", "", rows = 2, width = "100%", placeholder = "Alex Rivera^1^, Sam Lee^1,2^, Jordan Patel^2^"),
          textAreaInput("affiliations", "Affiliations (one per line, starting with its number)", "", rows = 3, width = "100%",
                        placeholder = "1 Department of Psychology, Northern Illinois University\n2 Department of Chemistry and Biochemistry, Northern Illinois University"),
          tags$details(class = "opts", tags$summary("Add a second logo (optional)"), div(class = "opts-body",
            p(class = "rule-note", icon("triangle-exclamation"), " PURC rule: ", tags$b("do not add any NIU logo"),
              " \u2014 the template\u2019s NIU logo is the only one allowed. Lab, funder or partner-institution logos are fine."),
            fileInput("logo2", NULL, accept = c("image/*", ".tif", ".tiff", ".svg"), width = "100%"))),
          uiOutput("logo2_info"),
          FORMAT_HELP)),
        unname(lapply(SECTION_KEYS, section_tab)),
        list(tabPanel(tagList(icon("plus"), "Add section"), value = "addsec", div()),
          tabPanel("Layout", value = "layout", icon = icon("table-columns"),
          radioButtons("layout", "Layout", setNames(names(LAYOUTS), vapply(LAYOUTS, `[[`, "", "label"))),
          actionButton("compare", "Compare layouts for my content", icon = icon("scale-balanced"), class = "btn-outline-primary btn-sm"),
          p(class = "help", "Scores every layout on what you've entered (text size and empty space) and recommends one."),
          sliderInput("body", "Text size range (pt)", min = 20, max = 44, value = c(24, 28), step = 1),
          p(class = "help", "Accepted PURC posters use about 24 pt body text and 17 pt references. Body text never goes below 20 pt; captions and tables never below 17 pt."),
          tags$details(class = "opts adv", tags$summary("Advanced settings"), div(class = "opts-body",
            checkboxInput("autofit", "Auto-adjust column widths and spacing to minimise blank space", TRUE),
            checkboxInput("grow", "Let figures grow into empty space", TRUE),
            checkboxInput("justify", "Justify body text (straight left and right edges)", TRUE),
            checkboxInput("balance", "Balance panel heights to fit their content", TRUE),
            checkboxInput("keep_empty", "Show empty figure/table slots as grey boxes", TRUE),
            checkboxInput("number", "Number figures and tables automatically (Figure 1., Table 1.)", TRUE),
            uiOutput("pkg_note"))),
          p(class = "help", icon("circle-info"), " To add an extra panel (Hypothesis, Future Directions\u2026), use ", tags$b("+ Add section"), " in the tab bar.")),
        tabPanel("Save / Load", value = "save", icon = icon("floppy-disk"),
          p("Save your work as a project file (.zip with your text and images) and load it later to keep editing."),
          downloadButton("save_proj", "Save project", class = "btn-outline-primary"),
          br(), br(),
          fileInput("load_proj", "Load a saved project (.zip)", accept = ".zip", width = "100%"),
          actionButton("reset", "Start over (clear everything)", class = "btn-outline-secondary btn-sm")))
      ))),
    column(7, class = "preview-col",
      div(class = "preview-bar",
        downloadButton("dl_pptx", "Download PowerPoint (.pptx)", class = "btn-primary"),
        actionButton("magic", "Magic arrange", icon = icon("wand-magic-sparkles"), class = "btn-warning",
                     title = "Try every layout and section split; pick the arrangement with the biggest text and least empty space"),
        actionButton("magic_undo", "Undo magic", icon = icon("rotate-left"), class = "btn-outline-secondary btn-sm", style = "display:none"),
        span(class = "help", textOutput("size_info", inline = TRUE))),
      uiOutput("progress"),
      uiOutput("warnings"),
      tags$details(class = "preview-help", tags$summary(icon("hand-pointer"), " Tips for the preview"),
        "Click a block to edit it. Drag a block to move it; drop it on the middle of another block to swap them. Drag a figure\u2019s small red corner to resize it."),
      div(class = "poster-wrap", uiOutput("preview")),
      div(class = "app-footer", "For educational use \u00b7 Please discuss your poster with your PI before use \u00b7 Ortiz Neuroscience Laboratory, NIU \u00b7 ",
          actionLink("about2", "About & disclaimer")))
  )
)

server <- function(input, output, session) {
  S <- new.env()
  S$proj <- default_project()
  adir <- tempfile("poster_assets_"); dir.create(adir)
  S$obs <- list()
  struct <- reactiveVal(0); content <- reactiveVal(0)
  bump_struct <- function() struct(isolate(struct()) + 1)
  bump <- function() content(isolate(content()) + 1)
  notify_err <- function(e) showNotification(conditionMessage(e), type = "error", duration = 8)

  get_b <- function(id) { loc <- find_block(S$proj, id); if (is.null(loc)) NULL else S$proj$sections[[loc[1]]]$blocks[[as.integer(loc[2])]] }
  set_f <- function(id, field, value) {
    loc <- find_block(S$proj, id); if (is.null(loc)) return()
    S$proj$sections[[loc[1]]]$blocks[[as.integer(loc[2])]][field] <- list(value)
  }
  add_asset <- function(a) {
    if (grepl("niu|northern[ _-]*illinois|huskie", a$name %||% "", ignore.case = TRUE) && !identical(a$kind, "table"))
      showNotification(HTML(sprintf("<b>Check this image:</b> \u201c%s\u201d looks like it may be an NIU logo. PURC rules allow only the NIU logo already on the template \u2014 please don\u2019t add another.", htmltools::htmlEscape(a$name))),
                       type = "warning", duration = 12)
    S$proj$assets[[a$id]] <- a; a$id
  }

  # ---- per-block observers -------------------------------------------------
  make_observers <- function(id) {
    b <- get_b(id); nm <- function(f) paste0(id, "_", f)
    o <- list()
    on <- function(f, handler) o[[length(o) + 1]] <<- observeEvent(input[[nm(f)]], handler(input[[nm(f)]]), ignoreInit = TRUE)
    on("up",   function(v) move_block(id, -1))
    on("down", function(v) move_block(id, +1))
    on("del",  function(v) remove_block(id))
    fields <- switch(b$type,
      text = c("text", "keep_lines", "boxed"), subheading = c("text", "style", "italic"), flow = c("steps", "caption"),
      figure = c("caption", "nonumber", "autogrow"), figrow = c("caption", "n", "labels", "label_color", "nonumber", "autogrow"),
      figpair = c("n", "nonumber", "autogrow"),
      figtext = c("text", "caption", "side", "text_boxes", "boxed", "keep_lines", "nonumber", "autogrow"),
      table = c("caption", "source", "header_mode"), refs = c("dois", "text", "sort"), callout = c("title", "text"), qr = c("label"), character(0))
    for (f in fields) local({
      ff <- f; on(ff, function(v) {
        if (identical(get_b(id)[[ff]], v)) return()
        set_f(id, ff, v); bump()
        if (ff %in% c("autogrow", "n", "source")) bump_struct()      # refresh the size label / caption boxes / table editor
      })
    })
    if (b$type == "figpair") for (k in 1:3) local({
      kk <- k; on(paste0("cap", kk), function(v) {
        caps <- as.list(unlist(get_b(id)$captions) %||% character(0)); length(caps) <- 3
        caps <- lapply(caps, function(x) x %||% "")
        if (identical(caps[[kk]], v)) return()
        caps[[kk]] <- v; set_f(id, "captions", caps); bump()
      })
    })
    if (b$type %in% c("figure", "figrow", "figpair", "figtext", "table", "qr", "equation")) on("size", function(v) {
      if (identical(as.numeric(get_b(id)$size %||% NA), as.numeric(v))) return()
      set_f(id, "size", v)
      # a size chosen by hand is kept: stop auto-growing this figure
      if (b$type %in% c("figure", "figrow", "figpair", "figtext") && !identical(get_b(id)$autogrow, FALSE)) { set_f(id, "autogrow", FALSE); bump_struct() }
      bump()
    })
    if (b$type %in% c("figure", "figtext")) on("file", function(v) tryCatch({
      set_f(id, "asset", add_asset(ingest_image(v$datapath, v$name, adir))); bump(); bump_struct()
    }, error = notify_err))
    if (b$type %in% c("figrow", "figpair")) on("files", function(v) tryCatch({
      ids <- lapply(seq_len(nrow(v)), function(i) add_asset(ingest_image(v$datapath[i], v$name[i], adir)))
      set_f(id, "assets", ids); set_f(id, "n", max(get_b(id)$n %||% 2, length(ids))); bump(); bump_struct()
    }, error = notify_err))
    if (b$type == "table") {
      on("file", function(v) tryCatch({
        set_f(id, "asset", add_asset(ingest_table(v$datapath, v$name, adir, get_b(id)$decimals %||% 2))); bump(); bump_struct()
      }, error = notify_err))
      on("decimals", function(v) { if (is.numeric(v) && !identical(get_b(id)$decimals, v)) { set_f(id, "decimals", v); bump() } })
      # typed-in table: the grid sends its cells as JSON while you type
      on("grid", function(v) {
        rows <- tryCatch(lapply(jsonlite::fromJSON(v, simplifyVector = FALSE), function(r) vapply(r, function(x) as.character(x %||% ""), "")),
                         error = function(e) NULL)
        if (!is.null(rows)) { set_f(id, "grid", rows); bump() }
      })
    }
    if (b$type == "refs") on("fetch", function(v) {
      dois <- parse_doi_list(input[[nm("dois")]])
      if (!length(dois)) { showNotification("Paste one or more DOIs first, e.g. 10.1038/nn1327", type = "warning"); return() }
      cur <- get_b(id)$text %||% ""
      have <- tolower(dois) %in% tolower(regmatches(cur, gregexpr("10\\.[^[:space:]]+", cur))[[1]])
      todo <- dois[!have]; added <- character(0); failed <- character(0)
      if (length(todo)) withProgress(message = "Looking up DOIs\u2026", value = 0, for (i in seq_along(todo)) {
        res <- crossref_lookup(todo[i])
        if (is.null(res)) failed <- c(failed, todo[i]) else added <- c(added, apa_reference_line(crossref_fields(res), todo[i]))
        incProgress(1 / length(todo))
      })
      if (length(added)) {
        new <- paste(c(if (nzchar(trimws(cur))) trimws(cur), added), collapse = "\n")
        set_f(id, "text", new); set_f(id, "dois", "")
        bump(); bump_struct()
      }
      msg <- c(if (length(added)) sprintf("Added %d reference%s in APA style. Double-check them, especially title capitalization.", length(added), if (length(added) > 1) "s" else ""),
               if (sum(have)) sprintf("%d DOI%s already in the list.", sum(have), if (sum(have) > 1) "s were" else " was"),
               if (length(failed)) sprintf("Couldn't find %s (or no internet connection). You can type %s in by hand.", paste(failed, collapse = ", "), if (length(failed) > 1) "them" else "it"))
      showNotification(paste(msg, collapse = " "), type = if (length(failed)) "warning" else "message", duration = 10)
    })
    if (b$type %in% c("qr", "equation")) {
      fld <- if (b$type == "qr") "url" else "latex"
      deb <- debounce(reactive(input[[nm(fld)]]), 900)
      o[[length(o) + 1]] <- observeEvent(deb(), {
        v <- deb(); set_f(id, fld, v)
        if (nzchar(trimws(v))) tryCatch({
          a <- if (b$type == "qr") make_qr(trimws(v), adir) else make_equation(v, adir)
          set_f(id, "asset", add_asset(a))
        }, error = function(e) { set_f(id, "asset", NULL); notify_err(e) })
        else set_f(id, "asset", NULL)
        bump()
      }, ignoreInit = TRUE)
    }
    S$obs[[id]] <- o
  }
  destroy_observers <- function(id) { for (x in S$obs[[id]]) x$destroy(); S$obs[[id]] <- NULL }

  move_block <- function(id, d) {
    loc <- find_block(S$proj, id); k <- loc[1]; j <- as.integer(loc[2])
    bl <- S$proj$sections[[k]]$blocks; t <- j + d
    if (t < 1 || t > length(bl)) return()
    bl[c(j, t)] <- bl[c(t, j)]; S$proj$sections[[k]]$blocks <- bl
    bump_struct()
  }
  remove_block <- function(id) {
    loc <- find_block(S$proj, id); if (is.null(loc)) return()
    S$proj$sections[[loc[1]]]$blocks[[as.integer(loc[2])]] <- NULL
    destroy_observers(id); bump_struct()
  }
  for (k in names(S$proj$sections)) for (b in S$proj$sections[[k]]$blocks) make_observers(b$id)

  # ---- block editor UI -----------------------------------------------------
  thumb <- function(a, h = 54) if (!is.null(a)) tags$img(src = a$thumb, height = paste0(h, "px"), class = "thumb")
  block_card <- function(b, j, n) {
    id <- b$id; nm <- function(f) paste0(id, "_", f)
    A <- S$proj$assets
    opts <- function(...) tags$details(class = "opts", tags$summary("More options"), div(class = "opts-body", ...))
    size_label <- function(b, lab = "Size") if (identical(b$autogrow, FALSE)) paste(lab, "(fixed)") else paste(lab, "(minimum \u2014 grows to fill empty space)")
    fig_opts <- function(b) tagList(
      checkboxInput(nm("autogrow"), "Auto-grow to fill empty space", !identical(b$autogrow, FALSE)),
      checkboxInput(nm("nonumber"), "Don't number this one (logo or decoration)", isTRUE(b$nonumber)))
    file_row <- function(input_tag, a) div(class = "file-row", input_tag, thumb(a), if (!is.null(a)) span(class = "help", a$name))
    body <- switch(b$type,
      text = tagList(
        textAreaInput(nm("text"), NULL, b$text, rows = 5, width = "100%", placeholder = "Type or paste text here…"),
        opts(checkboxInput(nm("keep_lines"), "Keep every line break (lists, references)", isTRUE(b$keep_lines)),
             selectInput(nm("boxed"), "Border", c("No border" = "none", "Dashed border" = "dashed", "Solid border" = "solid"), b$boxed %||% "none", selectize = FALSE))),
      subheading = tagList(
        textInput(nm("text"), NULL, b$text, width = "100%", placeholder = "e.g. Intracellular Calcium Dye Loading"),
        opts(selectInput(nm("style"), "Style", c("Outlined box" = "box", "Filled red bar" = "filled", "Plain with underline" = "plain"), b$style %||% "box", selectize = FALSE),
             checkboxInput(nm("italic"), "Italic", isTRUE(b$italic %||% TRUE)))),
      flow = tagList(
        textAreaInput(nm("steps"), "One step per line", b$steps, rows = 4, width = "100%",
                      placeholder = "Cut brain sections\nIncubate in oxygenated HBSS\nLoad calcium dye\nMove to the imaging chamber"),
        opts(textInput(nm("caption"), "Caption", b$caption, width = "100%"),
             p(class = "help", "To write a step over several lines, leave a blank line between steps instead. Rows balance automatically."))),
      figtext = tagList(
        file_row(fileInput(nm("file"), NULL, accept = c("image/*", ".tif", ".tiff", ".svg"), buttonLabel = "Choose image…", width = "100%"), A[[b$asset %||% ""]]),
        textAreaInput(nm("text"), "Text beside the image", b$text, rows = 3, width = "100%",
                      placeholder = "One box: type normally.  Separate boxes: one line per box (or leave a blank line between boxes)."),
        div(class = "inline-pair",
          radioButtons(nm("text_boxes"), "Text style", c("One text box" = "one", "A separate box for each line" = "each"),
                       selected = b$text_boxes %||% "one", inline = TRUE),
          radioButtons(nm("side"), "Text on the", c("Right" = "left", "Left" = "right", "Top" = "above", "Bottom" = "below"), selected = b$side %||% "left", inline = TRUE)),
        sliderInput(nm("size"), size_label(b, "Image size"), 20, 100, b$size, step = 5, post = "%", width = "100%"),
        opts(fig_opts(b),
             selectInput(nm("boxed"), "Text border", c("Dashed" = "dashed", "Solid" = "solid", "None" = "none"), b$boxed %||% "dashed", selectize = FALSE),
             textInput(nm("caption"), "Caption under the image", b$caption, width = "100%"))),
      figure = tagList(
        file_row(fileInput(nm("file"), NULL, accept = c("image/*", ".tif", ".tiff", ".svg"), buttonLabel = "Choose image…", width = "100%"), A[[b$asset %||% ""]]),
        textInput(nm("caption"), "Caption", b$caption, width = "100%", placeholder = "What should the viewer notice?"),
        sliderInput(nm("size"), size_label(b), 20, 100, b$size, step = 5, post = "%", width = "100%"),
        opts(fig_opts(b))),
      figrow = tagList(
        div(class = "file-row", fileInput(nm("files"), NULL, multiple = TRUE, accept = c("image/*", ".tif", ".tiff"), buttonLabel = "Choose 2–4 images…", width = "100%"),
            tagList(lapply(unlist(b$assets), function(i) thumb(A[[i]], 44)))),
        p(class = "help", "One figure made of several panels (A, B, C\u2026) sharing one caption and one figure number."),
        textInput(nm("caption"), "Caption", b$caption, width = "100%", placeholder = "(A) … (B) …"),
        sliderInput(nm("size"), size_label(b), 30, 100, b$size, step = 5, post = "%", width = "100%"),
        opts(fig_opts(b),
             numericInput(nm("n"), "Empty slots to show before images are added", b$n %||% 2, min = 2, max = 4, width = "100%"),
             checkboxInput(nm("labels"), "Label panels A, B, C…", isTRUE(b$labels)),
             radioButtons(nm("label_color"), "Label color", c("Black" = "black", "White (for dark images)" = "white"),
                          selected = b$label_color %||% "black", inline = TRUE))),
      figpair = {
        caps <- unlist(b$captions) %||% character(0)
        nslots <- max(b$n %||% 2, length(unlist(b$assets)))
        tagList(
          div(class = "file-row", fileInput(nm("files"), NULL, multiple = TRUE, accept = c("image/*", ".tif", ".tiff"), buttonLabel = "Choose 2\u20133 images\u2026", width = "100%"),
              tagList(lapply(unlist(b$assets), function(i) thumb(A[[i]], 44)))),
          p(class = "help", "Pick the images in the order you want them, left to right. Each gets its own figure number and caption."),
          lapply(seq_len(min(3, nslots)), function(k)
            textInput(nm(paste0("cap", k)), sprintf("Caption for image %d", k), if (k <= length(caps)) caps[[k]] else "", width = "100%")),
          sliderInput(nm("size"), size_label(b), 30, 100, b$size, step = 5, post = "%", width = "100%"),
          opts(fig_opts(b),
               numericInput(nm("n"), "Number of figures in the row", b$n %||% 2, min = 2, max = 3, width = "100%")))
      },
      table = {
        manual <- identical(b$source, "manual")
        a <- A[[b$asset %||% ""]]
        grid_json <- jsonlite::toJSON(lapply(b$grid %||% list(c("", "")), function(r) as.character(unlist(r))), auto_unbox = FALSE)
        tagList(
          radioButtons(nm("source"), NULL, c("Upload CSV / Excel" = "file", "Type it in" = "manual"), selected = if (manual) "manual" else "file", inline = TRUE),
          if (!manual) div(class = "file-row", fileInput(nm("file"), NULL, accept = c(".csv", ".xlsx", ".xls"), buttonLabel = "Choose CSV / Excel\u2026", width = "100%"),
              if (!is.null(a)) span(class = "help", sprintf("%s (%d rows \u00d7 %d columns)", a$name, nrow(a$cells), ncol(a$cells))))
          else tagList(
            div(id = nm("grid"), class = "tgrid", `data-grid` = as.character(grid_json)),
            p(class = "help", "Click a cell to type. Enter moves down. You can also paste a block of cells copied from Excel or Google Sheets.")),
          selectInput(nm("header_mode"), "First row", c("Column headers (one per column)" = "header",
                                                         "A title bar across the whole table" = "title",
                                                         "Just data (no header)" = "none"),
                      selected = b$header_mode %||% "header", width = "100%", selectize = FALSE),
          textInput(nm("caption"), "Caption (shown above the table)", b$caption, width = "100%"),
          sliderInput(nm("size"), "Size", 30, 100, b$size, step = 5, post = "%", width = "100%"),
          opts(numericInput(nm("decimals"), "Decimal places for numbers", b$decimals %||% 2, min = 0, max = 6, width = "160px"),
               p(class = "help", "Empty rows and columns are removed automatically. Up to 40 rows.")))
      },
      refs = tagList(
        textAreaInput(nm("dois"), "Paste DOIs (one per line)", b$dois %||% "", rows = 2, width = "100%",
                      placeholder = "10.1038/nn1327\nhttps://doi.org/10.1126/science.aac4785"),
        actionButton(nm("fetch"), "Fill in from DOIs", icon = icon("wand-magic-sparkles"), class = "btn-outline-primary btn-sm"),
        textAreaInput(nm("text"), "References (APA 7th edition, one per line)", b$text %||% "", rows = 6, width = "100%",
                      placeholder = "Filled in from your DOIs, or type your own:\nYoung, L. J., & Wang, Z. (2004). The neurobiology of pair bonding. *Nature Neuroscience, 7*(10), 1048\u20131054. https://doi.org/10.1038/nn1327"),
        { tips <- reference_tips(b); if (length(tips)) div(class = "ref-tips", lapply(tips, function(t) div(icon("lightbulb"), " ", t))) },
        opts(checkboxInput(nm("sort"), "Sort alphabetically by first author (APA)", !identical(b$sort, FALSE)),
             p(class = "help", "Use *asterisks* for italics (journal name and volume). In your poster text, cite as (Young & Wang, 2004)."))),
      callout = tagList(
        textAreaInput(nm("text"), NULL, b$text, rows = 3, width = "100%", placeholder = "Your take-home message in one or two sentences"),
        opts(textInput(nm("title"), "Callout title", b$title, width = "100%"))),
      qr = tagList(
        textInput(nm("url"), "Link", b$url, width = "100%", placeholder = "https://doi.org/…"),
        textInput(nm("label"), "Label under the code", b$label, width = "100%"),
        sliderInput(nm("size"), "Size", 15, 70, b$size, step = 5, post = "%", width = "100%")),
      equation = tagList(
        textInput(nm("latex"), "Equation (LaTeX)", b$latex, width = "100%", placeholder = "\\Delta F/F = (F - F_0)/F_0"),
        sliderInput(nm("size"), "Size", 50, 250, b$size, step = 10, post = "%", width = "100%")))
    div(class = paste("block-card", paste0("bt-", b$type)), id = paste0("card_", id),
      div(class = "block-head",
        span(class = "block-type", block_label(b$type)),
        div(class = "block-btns",
          actionButton(nm("up"), NULL, icon = icon("arrow-up"), class = "btn-light btn-xs", title = "Move up", disabled = if (j == 1) NA),
          actionButton(nm("down"), NULL, icon = icon("arrow-down"), class = "btn-light btn-xs", title = "Move down", disabled = if (j == n) NA),
          actionButton(nm("del"), NULL, icon = icon("xmark"), class = "btn-light btn-xs", title = "Remove block"))),
      body)
  }

  S$secobs <- list()
  setup_section <- function(k) {
    output[[paste0("blocks_", k)]] <- renderUI({
      struct()
      bl <- isolate(S$proj$sections[[k]]$blocks)
      if (!length(bl)) return(div(class = "empty-state",
        div(class = "empty-icon", icon("square-plus")),
        p(tags$b("This panel is empty."), " Its space is reserved on the poster \u2014 add what you need with the buttons below."),
        if (!is.na(SECTION_TIPS[k])) p(class = "help", icon("lightbulb"), " ", SECTION_TIPS[[k]])))
      split_ch <- c("Keep the whole section in one panel" = "")
      if (length(bl) > 1) split_ch <- c(split_ch, setNames(vapply(bl[-1], `[[`, "", "id"),
        sprintf("Continue in the next column from block %d (%s)", seq_along(bl)[-1], vapply(bl[-1], function(b) block_label(b$type), ""))))
      tagList(
        lapply(seq_along(bl), function(j) block_card(bl[[j]], j, length(bl))),
        tags$details(class = "opts split-row", open = if (nzchar(isolate(S$proj$sections[[k]]$split_at) %||% "")) NA,
          tags$summary("Section options"), div(class = "opts-body",
            selectInput(paste0("split_", k), "Too long for one column?", split_ch,
                        selected = isolate(S$proj$sections[[k]]$split_at) %||% "", width = "100%", selectize = FALSE))))
    })
    o <- list(
      observeEvent(input[[paste0("add_", k)]], {
        b <- new_block(input[[paste0("addtype_", k)]])
        S$proj$sections[[k]]$blocks[[length(S$proj$sections[[k]]$blocks) + 1]] <- b
        make_observers(b$id); bump_struct()
      }),
      observeEvent(input[[paste0("heading_", k)]], { S$proj$sections[[k]]$heading <- input[[paste0("heading_", k)]]; bump() }, ignoreInit = TRUE),
      observeEvent(input[[paste0("visible_", k)]], { S$proj$sections[[k]]$visible <- isTRUE(input[[paste0("visible_", k)]]); bump() }, ignoreInit = TRUE),
      observeEvent(input[[paste0("split_", k)]], {
        v <- input[[paste0("split_", k)]]
        if (!identical(v, S$proj$sections[[k]]$split_at %||% "")) { S$proj$sections[[k]]$split_at <- if (nzchar(v)) v else NULL; bump() }
      }, ignoreInit = TRUE))
    if (!k %in% SECTION_KEYS) o <- c(o, list(
      observeEvent(input[[paste0("place_", k)]], { S$proj$sections[[k]]$place <- input[[paste0("place_", k)]]; bump() }, ignoreInit = TRUE),
      observeEvent(input[[paste0("delbox_", k)]], remove_custom(k), ignoreInit = TRUE)))
    S$secobs[[k]] <- o
  }
  lapply(SECTION_KEYS, setup_section)

  # ---- custom boxes ------------------------------------------------------------
  insert_custom_tab <- function(k, select = FALSE) {
    sec <- S$proj$sections[[k]]
    others <- setdiff(custom_keys(S$proj), k)
    target <- if (length(others) && match(k, names(S$proj$sections)) > match(others[1], names(S$proj$sections)))
      others[max(which(match(others, names(S$proj$sections)) < match(k, names(S$proj$sections))))] else "refs"
    insertTab("tabs", section_tab(k, sec$heading, custom = TRUE, place = sec$place), target = target, position = "after", select = select)
    setup_section(k)
  }
  remove_custom <- function(k) {
    removeTab("tabs", k)
    for (b in S$proj$sections[[k]]$blocks) destroy_observers(b$id)
    for (x in S$secobs[[k]]) x$destroy(); S$secobs[[k]] <- NULL
    S$proj$sections[[k]] <- NULL
    bump_struct()
  }
  S$last_tab <- "header"
  observeEvent(input$tabs, {
    if (!identical(input$tabs, "addsec")) { S$last_tab <- input$tabs; return() }
    updateTabsetPanel(session, "tabs", selected = S$last_tab)
    sugg <- c("Hypothesis", "Objective", "Future Directions", "Device Fabrication", "Limitations", "Contact")
    showModal(modalDialog(title = "Add a section", easyClose = TRUE,
      p(class = "help", "A new panel with its own heading. It gets a tab like the other sections and can hold any blocks."),
      textInput("new_box_name", "Section name", "", width = "100%", placeholder = "e.g. Hypothesis"),
      div(class = "sugg-row", lapply(sugg, function(x) tags$button(type = "button", class = "btn btn-light btn-sm sugg",
        onclick = sprintf("Shiny.setInputValue('new_box_name', '%s'); document.getElementById('new_box_name').value = '%s';", x, x), x))),
      selectInput("new_box_place", "Where on the poster?", PLACE_CHOICES, selected = "below:goal", width = "100%", selectize = FALSE),
      footer = tagList(modalButton("Cancel"), actionButton("add_box", "Add section", icon = icon("plus"), class = "btn-primary"))))
  }, ignoreInit = TRUE)
  observeEvent(input$add_box, {
    removeModal()
    k <- new_id("c")
    nm <- trimws(input$new_box_name %||% ""); if (!nzchar(nm)) nm <- "New section"
    S$proj$sections[[k]] <- new_custom_section(nm, input$new_box_place)
    for (b in S$proj$sections[[k]]$blocks) make_observers(b$id)
    insert_custom_tab(k, select = TRUE)
    bump_struct()
  })

  # ---- header & settings ---------------------------------------------------
  for (f in c("title", "authors", "affiliations")) local({
    ff <- f; observeEvent(input[[ff]], { S$proj$meta[[ff]] <- input[[ff]]; bump() }, ignoreInit = TRUE)
  })
  observeEvent(input$logo2, tryCatch({ S$proj$meta$logo2 <- add_asset(ingest_image(input$logo2$datapath, input$logo2$name, adir)); bump(); bump_struct() }, error = notify_err))
  output$logo2_info <- renderUI({
    struct(); a <- isolate(S$proj$assets[[S$proj$meta$logo2 %||% ""]])
    if (!is.null(a)) div(class = "file-row", thumb(a, 40), actionLink("logo2_rm", "Remove second logo"))
  })
  observeEvent(input$logo2_rm, { S$proj$meta$logo2 <- NULL; bump(); bump_struct() })
  observeEvent(input$layout, { S$proj$settings$layout <- input$layout; bump() }, ignoreInit = TRUE)
  observeEvent(input$grow, { S$proj$settings$grow <- input$grow; bump() }, ignoreInit = TRUE)
  observeEvent(input$compare, {
    df <- withProgress(message = "Trying each layout\u2026", value = 0.5, compare_layouts(S$proj))
    cur <- S$proj$settings$layout %||% "classic"
    rows <- lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, ]
      tags$tr(class = if (i == 1) "best" else NULL,
        tags$td(r$label, if (i == 1) tags$span(class = "badge bg-success ms-1", "Recommended"), if (r$key == cur) tags$span(class = "badge bg-secondary ms-1", "Current")),
        tags$td(paste(r$body, "pt")), tags$td(paste0(r$empty, "%")),
        tags$td(if (r$warnings) paste(r$warnings, "warning(s)") else "\u2014"),
        tags$td(if (r$key != cur) actionButton(paste0("use_layout_", r$key), "Use", class = "btn-sm btn-primary",
                  onclick = sprintf("Shiny.setInputValue('use_layout','%s',{priority:'event'})", r$key))))
    })
    showModal(modalDialog(title = "Layouts compared on your content", size = "l", easyClose = TRUE,
      p("Bigger text is easier to read from a distance; less empty space looks more finished."),
      tags$table(class = "table table-sm cmp-table", tags$thead(tags$tr(tags$th("Layout"), tags$th("Body text"), tags$th("Empty space"), tags$th("Issues"), tags$th(""))),
                 tags$tbody(rows)),
      footer = modalButton("Close")))
  })
  observeEvent(input$use_layout, { updateRadioButtons(session, "layout", selected = input$use_layout); removeModal() })
  observeEvent(input$autofit, { S$proj$settings$autofit <- input$autofit; bump() }, ignoreInit = TRUE)
  observeEvent(input$justify, { S$proj$settings$justify <- input$justify; bump() }, ignoreInit = TRUE)
  observeEvent(input$balance, { S$proj$settings$balance <- input$balance; bump() }, ignoreInit = TRUE)
  observeEvent(input$body, { S$proj$settings$body_min <- input$body[1]; S$proj$settings$body_max <- input$body[2]; bump() }, ignoreInit = TRUE)
  observeEvent(input$keep_empty, { S$proj$settings$keep_empty <- input$keep_empty; bump() }, ignoreInit = TRUE)
  observeEvent(input$number, { S$proj$settings$number <- input$number; bump() }, ignoreInit = TRUE)
  observeEvent(input$quick_add, {
    v <- input$quick_add; k <- v$key
    if (is.null(S$proj$sections[[k]])) return()
    b <- new_block(v$type)
    if (k == "refs" && v$type == "text") b$keep_lines <- TRUE
    S$proj$sections[[k]]$blocks[[length(S$proj$sections[[k]]$blocks) + 1]] <- b
    make_observers(b$id); bump_struct()
    session$sendCustomMessage("scroll_card", b$id)
  })
  observeEvent(input$preview_click, updateTabsetPanel(session, "tabs", selected = sub("__cont$", "", input$preview_click)))
  observeEvent(input$preview_block, {
    updateTabsetPanel(session, "tabs", selected = sub("__cont$", "", input$preview_block$key))
    session$sendCustomMessage("scroll_card", input$preview_block$id)
  })

  # ---- drag & drop from the preview ------------------------------------------
  observeEvent(input$drag_move, {
    v <- input$drag_move; id <- v$id
    src <- find_block(S$proj, id); if (is.null(src)) return()
    b <- S$proj$sections[[src[1]]]$blocks[[as.integer(src[2])]]
    if (identical(v$mode, "swap")) {
      tg <- find_block(S$proj, v$target); if (is.null(tg)) return()
      tb <- S$proj$sections[[tg[1]]]$blocks[[as.integer(tg[2])]]
      S$proj$sections[[src[1]]]$blocks[[as.integer(src[2])]] <- tb
      S$proj$sections[[tg[1]]]$blocks[[as.integer(tg[2])]] <- b
    } else {
      S$proj$sections[[src[1]]]$blocks[[as.integer(src[2])]] <- NULL
      if (identical(v$mode, "append") || is.null(v$target)) {
        k <- sub("__cont$", "", v$key); S$proj$sections[[k]]$blocks <- c(S$proj$sections[[k]]$blocks, list(b))
      } else {
        tg <- find_block(S$proj, v$target); k <- tg[1]; j <- as.integer(tg[2])
        S$proj$sections[[k]]$blocks <- append(S$proj$sections[[k]]$blocks, list(b), after = if (identical(v$mode, "before")) j - 1 else j)
      }
    }
    bump_struct()
  })
  observeEvent(input$drag_resize, {
    v <- input$drag_resize; b <- get_b(v$id); if (is.null(b)) return()
    rng <- SIZE_RANGE[[b$type]] %||% c(20, 100)
    set_f(v$id, "size", max(rng[1], min(rng[2], round(as.numeric(v$size) / 5) * 5)))
    if (b$type %in% c("figure", "figrow", "figpair", "figtext")) set_f(v$id, "autogrow", FALSE)   # keep the size you dragged to
    bump_struct()
  })

  # ---- missing-package notices -------------------------------------------------
  # Only a missing *required* package gets a banner; optional ones are a quiet
  # note under Advanced settings (and a clear message if someone uses that feature).
  output$pkg_banner <- renderUI({
    req_pk <- c("officer", "flextable", "readxl", "xml2", "jsonlite")
    miss <- req_pk[!vapply(req_pk, requireNamespace, logical(1), quietly = TRUE)]
    if (!length(miss)) return(NULL)
    div(class = "warn-box", icon("box-open"), div(p(tags$b("Some required R packages are missing: "), paste(miss, collapse = ", ")),
      p("Run this in R, then restart the app: ", tags$code(sprintf("install.packages(c(%s))", paste0('"', miss, '"', collapse = ", "))))))
  })
  output$pkg_note <- renderUI({
    opt_pk <- c(magick = "TIFF/GIF images", qrcode = "QR codes", latex2exp = "equations")
    miss <- names(opt_pk)[!vapply(names(opt_pk), requireNamespace, logical(1), quietly = TRUE)]
    if (length(miss)) p(class = "help", icon("circle-info"), sprintf(" Optional extras not installed: %s. Install with ", paste(sprintf("%s (%s)", miss, opt_pk[miss]), collapse = ", ")),
                        tags$code(sprintf("install.packages(c(%s))", paste0('"', miss, '"', collapse = ", "))))
  })

  # ---- about & disclaimer ------------------------------------------------------------
  observeEvent(input$about2, session$sendCustomMessage("run_js", "document.getElementById('about').click()"))
  observeEvent(input$about, showModal(modalDialog(title = "About the Poster Builder", easyClose = TRUE, size = "m",
    p("A free tool for students preparing posters for the ", tags$b("Psychology Undergraduate Research Conference"),
      ". It lays out a 40 \u00d7 32 in. poster in the NIU template style and exports an editable PowerPoint file."),
    p("Developed by the ", tags$b("Ortiz Neuroscience Laboratory"), ", Department of Psychology, Northern Illinois University."),
    h6("Disclaimer"),
    tags$ul(class = "about-list",
      tags$li("This tool is provided for ", tags$b("educational purposes"), " only, as is, without warranty of any kind."),
      tags$li(tags$b("Students: please discuss your poster with your PI or faculty mentor before using or submitting it."),
              " They should approve the content, data, figures, authorship and affiliations."),
      tags$li("You are responsible for the accuracy of your content and for following the conference\u2019s poster guidelines and your lab\u2019s data-sharing rules."),
      tags$li("This is not an official Northern Illinois University product. Use of the NIU template and logo should follow university branding guidelines."),
      tags$li(tags$b("PURC rules: "), "keep the template\u2019s NIU logo as the only NIU logo (no additional NIU logos), don\u2019t put a dark color behind it, and keep the vertical 40 \u00d7 32 in. format (the app always does). Lab and funder logos are fine."),
      tags$li("Files you upload are used only to build your poster during your session and are deleted when you close the app. Use ", tags$b("Save project"), " to keep your work.")),
    footer = modalButton("Close"))))
  session$onSessionEnded(function() unlink(adir, recursive = TRUE))

  # ---- first-visit welcome -------------------------------------------------------
  output$welcome <- renderUI(div(id = "welcome_card", class = "welcome", style = "display:none",
    div(class = "welcome-inner",
      tags$button(type = "button", class = "btn-close", `aria-label` = "Close", onclick = "dismissWelcome()"),
      h4("Make your poster in three steps"),
      div(class = "welcome-steps",
        div(class = "wstep", span(class = "wnum", "1"), div(tags$b("Add your title and authors"), br(), span("in the Header tab."))),
        div(class = "wstep", span(class = "wnum", "2"), div(tags$b("Fill in each section"), br(), span("Click a panel on the poster, then add text, figures or tables."))),
        div(class = "wstep", span(class = "wnum", "3"), div(tags$b("Press Magic arrange, then Download"), br(), span("You get an editable PowerPoint file.")))),
      p(class = "welcome-note", icon("user-graduate"), " For educational use. ", tags$b("Please discuss your poster with your PI or faculty mentor before you use or submit it.")),
      tags$button(type = "button", class = "btn btn-primary btn-sm", onclick = "dismissWelcome()", "Let's start"))))

  # ---- progress strip --------------------------------------------------------------
  output$progress <- renderUI({
    lay(); P <- S$proj
    keys <- Filter(function(k) isTRUE(P$sections[[k]]$visible), names(P$sections))
    started <- vapply(keys, function(k) length(active_blocks(P$sections[[k]], P$assets, FALSE)) > 0, logical(1))
    has_title <- nzchar(trimws(P$meta$title %||% ""))
    div(class = "progress-strip",
      span(class = paste("pdot", if (has_title) "done"), title = "Title", onclick = "Shiny.setInputValue('preview_click','header',{priority:'event'})", "Title"),
      lapply(keys, function(k) span(class = paste("pdot", if (started[[k]]) "done"), title = P$sections[[k]]$heading,
        onclick = sprintf("Shiny.setInputValue('preview_click','%s',{priority:'event'})", k), P$sections[[k]]$heading)),
      span(class = "pcount", sprintf("%d of %d sections started", sum(started), length(started))))
  })

  # ---- preview ---------------------------------------------------------------
  content_d <- debounce(content, 350)
  lay <- reactive({ content_d(); struct(); compute_layout(S$proj) })
  output$preview <- renderUI(HTML(preview_html(S$proj, lay())))
  output$size_info <- renderText(sprintf("Body text: %d pt", lay()$body))
  output$warnings <- renderUI({
    w <- lay()$warnings
    w <- w[!grepl("^Still empty", w)]
    if (length(w)) div(class = "note-box", icon("circle-info"), tags$ul(lapply(w, tags$li)))
  })

  slug <- function() { t <- gsub("[*^~]", "", S$proj$meta$title %||% ""); t <- gsub("[^A-Za-z0-9]+", "_", substr(t, 1, 40)); if (!nzchar(gsub("_", "", t))) "poster" else t }
  output$dl_pptx <- downloadHandler(
    filename = function() paste0(slug(), "_poster.pptx"),
    content = function(file) withProgress(message = "Building PowerPoint…", value = 0.5, {
      build_pptx(S$proj, compute_layout(S$proj), adir, TEMPLATE, file)
    }))
  output$save_proj <- downloadHandler(
    filename = function() paste0(slug(), "_project.zip"),
    content = function(file) save_project(S$proj, adir, file))

  # ---- load / reset ------------------------------------------------------------
  apply_project <- function(p) {
    for (id in names(S$obs)) destroy_observers(id)
    for (k in custom_keys(S$proj)) { removeTab("tabs", k); for (x in S$secobs[[k]]) x$destroy(); S$secobs[[k]] <- NULL }
    S$proj <- p
    for (k in names(p$sections)) for (b in p$sections[[k]]$blocks) make_observers(b$id)
    for (k in custom_keys(p)) insert_custom_tab(k)
    for (f in c("title", "authors", "affiliations")) updateTextAreaInput(session, f, value = p$meta[[f]] %||% "")
    for (k in SECTION_KEYS) {
      updateTextInput(session, paste0("heading_", k), value = p$sections[[k]]$heading)
      updateCheckboxInput(session, paste0("visible_", k), value = isTRUE(p$sections[[k]]$visible))
    }
    st <- p$settings
    updateRadioButtons(session, "layout", selected = st$layout); updateCheckboxInput(session, "balance", value = isTRUE(st$balance)); updateCheckboxInput(session, "justify", value = !identical(st$justify, FALSE)); updateCheckboxInput(session, "autofit", value = !identical(st$autofit, FALSE)); updateCheckboxInput(session, "grow", value = !identical(st$grow, FALSE))
    updateSliderInput(session, "body", value = c(st$body_min, st$body_max))
    updateCheckboxInput(session, "keep_empty", value = isTRUE(st$keep_empty)); updateCheckboxInput(session, "number", value = isTRUE(st$number))
    bump_struct(); bump()
  }
  observeEvent(input$load_proj, tryCatch({
    apply_project(load_project(input$load_proj$datapath, adir))
    showNotification("Project loaded.", type = "message")
  }, error = notify_err))
  observeEvent(input$reset, showModal(modalDialog("Clear the whole poster and start from the template?",
    footer = tagList(modalButton("Cancel"), actionButton("reset_ok", "Clear everything", class = "btn-danger")))))
  observeEvent(input$reset_ok, { removeModal(); apply_project(default_project()) })

  # ---- magic arrange ---------------------------------------------------------------
  S$magic <- NULL; S$undo <- NULL
  mini_preview <- function(p, lay) div(class = "magic-wrap", HTML(preview_html(p, lay)))
  observeEvent(input$magic, {
    res <- withProgress(message = "Magic arrange", value = 0, magic_arrange(S$proj, function(v, msg) setProgress(v, detail = msg)))
    S$magic <- res
    b <- res$before$stats
    if (!length(res$options)) {
      showModal(modalDialog(title = "Already well arranged", easyClose = TRUE,
        p(sprintf("No arrangement beat the current one (%d pt text, %d%% empty space). To fill remaining space, add content or raise the text size range in the Layout tab.", b$body, b$empty))))
      return()
    }
    cards <- lapply(seq_along(res$options), function(i) {
      o <- res$options[[i]]
      div(class = paste("magic-card", if (i == 1) "best"),
        div(class = "magic-title", sprintf("Option %d", i), if (i == 1) span(class = "badge bg-success ms-1", "Best")),
        mini_preview(o$p, o$lay),
        div(class = "magic-stats", sprintf("%d pt text \u00b7 %d%% empty", o$stats$body, o$stats$empty)),
        tags$ul(class = "magic-changes", lapply(o$desc, tags$li)),
        actionButton(paste0("magic_apply_", i), "Apply this option", class = "btn-primary btn-sm",
                     onclick = sprintf("Shiny.setInputValue('magic_apply', %d, {priority:'event'})", i)))
    })
    showModal(modalDialog(title = "Magic arrange", size = "xl", easyClose = TRUE,
      p(class = "help", sprintf("Current poster: %d pt text, %d%% empty space. Each option below was laid out with your actual content. Nothing changes until you apply one, and you can undo it.", b$body, b$empty)),
      div(class = "magic-grid", cards), footer = modalButton("Keep my current arrangement")))
  })
  observeEvent(input$magic_apply, {
    o <- S$magic$options[[input$magic_apply]]; if (is.null(o)) return()
    S$undo <- S$proj
    removeModal(); apply_project(o$p)
    shinyjs_show <- "document.getElementById('magic_undo').style.display='inline-block'"
    session$sendCustomMessage("run_js", shinyjs_show)
    showNotification(sprintf("Applied: %d pt text, %d%% empty space. Use 'Undo magic' to go back.", o$stats$body, o$stats$empty), type = "message", duration = 8)
  })
  observeEvent(input$magic_undo, {
    if (is.null(S$undo)) return()
    apply_project(S$undo); S$undo <- NULL
    session$sendCustomMessage("run_js", "document.getElementById('magic_undo').style.display='none'")
    showNotification("Restored the arrangement from before Magic arrange.", type = "message")
  })
}

shinyApp(ui, server)
