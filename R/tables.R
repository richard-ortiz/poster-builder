# ---------------------------------------------------------------------------
# Tables: uploaded (CSV / Excel) or typed into the in-app grid.
# Everything is kept as a plain text matrix of cells; the header style decides
# how the first row is used:
#   "header" - first row labels each column (blank header cells stay blank)
#   "title"  - first row becomes one bar spanning the whole table
#   "none"   - every row is data
# ---------------------------------------------------------------------------

MAX_TABLE_ROWS <- 40

# Drop rows / columns that are completely empty; NA -> ""
clean_cells <- function(m) {
  if (is.null(m) || !length(m)) return(matrix("", 0, 0))
  m <- as.matrix(m); m[is.na(m)] <- ""
  storage.mode(m) <- "character"
  m[] <- trimws(m)
  keep_r <- apply(m, 1, function(r) any(nzchar(r)))
  keep_c <- apply(m, 2, function(c) any(nzchar(c)))
  m[keep_r, keep_c, drop = FALSE]
}

# Read a CSV / Excel file without guessing headers, as text
read_table_cells <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  df <- if (ext %in% c("xlsx", "xls")) {
    suppressMessages(readxl::read_excel(path, sheet = 1, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
  } else {
    utils::read.csv(path, header = FALSE, colClasses = "character", na.strings = character(0),
                    check.names = FALSE, encoding = "UTF-8", strip.white = TRUE)
  }
  clean_cells(as.matrix(as.data.frame(df, stringsAsFactors = FALSE)))
}

write_table_cells <- function(m, file) {
  utils::write.table(m, file, sep = ",", col.names = FALSE, row.names = FALSE, qmethod = "double", fileEncoding = "UTF-8")
}

load_table_cells <- function(file) {
  clean_cells(as.matrix(utils::read.csv(file, header = FALSE, colClasses = "character", na.strings = character(0),
                                        check.names = FALSE, encoding = "UTF-8")))
}

# The in-app grid is stored as a list of rows (character vectors of any length)
grid_cells <- function(grid) {
  rows <- lapply(grid %||% list(), function(r) as.character(unlist(r)))
  if (!length(rows)) return(matrix("", 0, 0))
  w <- max(1, vapply(rows, length, 1L))
  clean_cells(do.call(rbind, lapply(rows, function(r) c(r, rep("", w - length(r)))[seq_len(w)])))
}

# Raw cells for a table block (uploaded file or typed grid); NULL when empty
table_cells <- function(b, assets) {
  m <- if (identical(b$source, "manual")) grid_cells(b$grid) else {
    a <- assets[[b$asset %||% ""]]
    if (is.null(a)) NULL else (a$cells %||% matrix("", 0, 0))
  }
  if (is.null(m) || !length(m) || !any(nzchar(m))) NULL else m
}

# Split into title / header / body and format numbers
prep_table <- function(m, header_mode = "header", decimals = 2) {
  header <- NULL; title <- NULL
  if (nrow(m) >= 1 && identical(header_mode, "header")) { header <- m[1, ]; m <- m[-1, , drop = FALSE] }
  if (nrow(m) >= 1 && identical(header_mode, "title")) {
    title <- paste(m[1, nzchar(m[1, ])], collapse = " "); m <- m[-1, , drop = FALSE]
  }
  if (nrow(m) > MAX_TABLE_ROWS) m <- m[seq_len(MAX_TABLE_ROWS), , drop = FALSE]
  # columns whose filled cells are all numbers get the decimals setting
  for (j in seq_len(ncol(m))) {
    v <- m[, j]; nz <- nzchar(v)
    if (!any(nz)) next
    num <- suppressWarnings(as.numeric(gsub(",", "", v[nz], fixed = TRUE)))
    if (!anyNA(num)) m[nz, j] <- format_table(data.frame(x = num), decimals)$x
  }
  list(body = m, header = header, title = title, ncol = max(ncol(m), length(header), 1))
}

table_placeholder <- function() list(body = matrix("…", 2, 2), header = c("Column", "Column"), title = NULL, ncol = 2)
