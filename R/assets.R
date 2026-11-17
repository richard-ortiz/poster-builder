# ---------------------------------------------------------------------------
# Assets: uploaded images, tables, generated QR codes and equations.
# Each asset is stored as a file in the session's asset folder and described by
# list(id, kind, file, name, w, h, thumb [data URI], data [tables only]).
# ---------------------------------------------------------------------------

new_id <- function(prefix = "a") paste0(prefix, paste(sample(c(letters, 0:9), 8, TRUE), collapse = ""))

has_magick <- function() requireNamespace("magick", quietly = TRUE)

# Read width/height straight from a PNG or JPEG header (no packages needed).
image_dims <- function(path) {
  con <- file(path, "rb"); on.exit(close(con))
  hdr <- readBin(con, "raw", 26)
  be <- function(b) sum(as.integer(b) * 256^(rev(seq_along(b)) - 1))
  if (length(hdr) >= 24 && identical(hdr[1:4], as.raw(c(0x89, 0x50, 0x4E, 0x47))))
    return(c(w = be(hdr[17:20]), h = be(hdr[21:24])))
  if (length(hdr) >= 2 && identical(hdr[1:2], as.raw(c(0xFF, 0xD8)))) {
    seek(con, 2)
    repeat {
      m <- readBin(con, "raw", 4); if (length(m) < 4) break
      if (m[1] != as.raw(0xFF)) break
      len <- be(m[3:4]); mk <- as.integer(m[2])
      if (mk %in% c(0xC0:0xC3, 0xC5:0xC7, 0xC9:0xCB, 0xCD:0xCF)) {
        d <- readBin(con, "raw", 5); return(c(w = be(d[4:5]), h = be(d[2:3])))
      }
      seek(con, len - 2, origin = "current")
    }
  }
  stop("Could not read this image. Please save it as PNG or JPEG.")
}

# Preview thumbnail as a data URI (downscaled when magick is available, original bytes otherwise)
thumb_from_file <- function(path, max_px = 700) {
  if (has_magick()) {
    img <- magick::image_read(path)[1]
    info <- magick::image_info(img)
    if (max(info$width, info$height) > max_px) img <- magick::image_scale(img, paste0(max_px, "x", max_px))
    img <- magick::image_background(img, "white")
    return(paste0("data:image/jpeg;base64,", jsonlite::base64_enc(magick::image_write(img, format = "jpeg", quality = 82))))
  }
  ext <- tolower(tools::file_ext(path))
  mime <- if (ext %in% c("jpg", "jpeg")) "image/jpeg" else "image/png"
  paste0("data:", mime, ";base64,", jsonlite::base64_enc(readBin(path, "raw", file.info(path)$size)))
}

# PNG/JPEG are kept as-is at full resolution. Other formats (TIFF, GIF, BMP, WEBP, SVG)
# are converted to PNG with magick, which is optional.
ingest_image <- function(path, name, dir, kind = "image") {
  ext <- tolower(tools::file_ext(name))
  id <- new_id()
  if (ext %in% c("png", "jpg", "jpeg")) {
    out <- file.path(dir, paste0(id, ".", ifelse(ext == "jpeg", "jpg", ext)))
    file.copy(path, out, overwrite = TRUE)
    d <- image_dims(out)
  } else {
    if (!has_magick()) stop(sprintf("'.%s' images need the 'magick' R package. Run install.packages(\"magick\") and restart the app, or save the image as PNG or JPEG.", ext))
    img <- magick::image_read(path)[1]
    out <- file.path(dir, paste0(id, ".png"))
    magick::image_write(img, out, format = "png")
    info <- magick::image_info(img); d <- c(w = info$width, h = info$height)
  }
  list(id = id, kind = kind, file = basename(out), name = name, w = unname(d["w"]), h = unname(d["h"]),
       thumb = thumb_from_file(out))
}

ingest_table <- function(path, name, dir, decimals = 2) {
  ext <- tolower(tools::file_ext(name))
  df <- if (ext %in% c("xlsx", "xls")) as.data.frame(readxl::read_excel(path, sheet = 1), check.names = FALSE)
        else utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (nrow(df) > 40) df <- df[1:40, , drop = FALSE]
  id <- new_id()
  out <- file.path(dir, paste0(id, ".csv"))
  utils::write.csv(df, out, row.names = FALSE)
  list(id = id, kind = "table", file = basename(out), name = name, raw = df,
       data = format_table(df, decimals), decimals = decimals, w = 1, h = 1)
}

format_table <- function(df, decimals = 2) {
  df[] <- lapply(df, function(col) {
    if (is.numeric(col)) {
      all_int <- all(is.na(col) | (col == round(col) & abs(col) < 1e15))
      vapply(col, function(v) {
        if (is.na(v)) return("")
        if (all_int) return(formatC(v, format = "d", big.mark = ","))
        # keep tiny values (e.g. p-values) readable instead of rounding them to 0.00
        if (abs(v) < 10^-decimals) return(formatC(signif(v, 1), format = "fg"))
        formatC(v, format = "f", digits = decimals, big.mark = ",")
      }, character(1))
    } else { col <- as.character(col); col[is.na(col)] <- ""; col }
  })
  df
}

# QR code -> PNG asset, drawn with base R graphics. Placeholder if 'qrcode' is missing.
make_qr <- function(url, dir) {
  id <- new_id(); out <- file.path(dir, paste0(id, ".png"))
  if (requireNamespace("qrcode", quietly = TRUE)) {
    m <- as.matrix(qrcode::qr_code(url, ecl = "M")) * 1
    m <- rbind(matrix(0, 4, ncol(m) + 8), cbind(matrix(0, nrow(m), 4), m, matrix(0, nrow(m), 4)), matrix(0, 4, ncol(m) + 8))
    px <- ncol(m) * 20
    grDevices::png(out, width = px, height = px, bg = "white")
    graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new(); graphics::plot.window(c(0, 1), c(0, 1), xaxs = "i", yaxs = "i")
    graphics::rasterImage(as.raster(ifelse(m == 1, "#000000", "#FFFFFF")), 0, 0, 1, 1, interpolate = FALSE)
    grDevices::dev.off()
  } else {
    grDevices::png(out, width = 600, height = 600, bg = "white")
    graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new(); graphics::box(col = "#999999", lwd = 8)
    graphics::text(0.5, 0.5, "QR", cex = 9, col = "#999999")
    grDevices::dev.off()
  }
  d <- image_dims(out)
  list(id = id, kind = "qr", file = basename(out), name = url, w = unname(d["w"]), h = unname(d["h"]), thumb = thumb_from_file(out, 300))
}

# LaTeX equation -> transparent PNG via latex2exp (plotmath), sized to fit the expression exactly.
make_equation <- function(latex, dir) {
  if (!requireNamespace("latex2exp", quietly = TRUE)) stop("Equations need the 'latex2exp' R package: install.packages(\"latex2exp\").")
  expr <- latex2exp::TeX(paste0("$", gsub("^\\$+|\\$+$", "", latex), "$"))
  id <- new_id(); out <- file.path(dir, paste0(id, ".png"))
  cex <- 2.6; res <- 300
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 4000, height = 1000, res = res)
  graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
  w <- graphics::strwidth(expr, units = "inches", cex = cex); h <- graphics::strheight(expr, units = "inches", cex = cex)
  grDevices::dev.off(); unlink(tmp)
  W <- ceiling((w + 0.15) * res); H <- ceiling((h * 1.6 + 0.15) * res)
  grDevices::png(out, width = W, height = H, res = res, bg = "transparent")
  graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
  graphics::text(0.5, 0.5, expr, cex = cex)
  grDevices::dev.off()
  list(id = id, kind = "equation", file = basename(out), name = latex, w = W, h = H, thumb = thumb_from_file(out, 600))
}

asset_path <- function(a, dir) file.path(dir, a$file)
