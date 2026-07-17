# =====================================================================
# Plate Reader Kinetics Explorer  —  Shiny app  (v2)
# ---------------------------------------------------------------------
# Upload a BioTek Synergy kinetic export and get time-courses, AUC
# rankings, QC and a plate heatmap. Name the wells inside the app on a
# clickable 96/384-well map — no separate map file needed, no code editing.
#
# RUN IT:
#   1. Put this file in a folder, name it  app.R
#   2. In RStudio: open it, click "Run App"
#      or from the console:  shiny::runApp("path/to/folder")
#
# WHAT IT UNDERSTANDS (auto-detected, nothing hard-coded):
#   * Sheets are listed from the file; kinetic blocks are found by scanning
#     for the "Time" header cell.
#   * Multiple read channels (e.g. "Read 3:480,510" and "Read 4:600") are
#     detected and offered as Signal / Density channels.
#   * Channels split across several tables (the 96-column export wrap) are
#     stitched back together.
#   * Elapsed time that wraps past 24:00:00 is unwrapped to real hours.
#   * "OVRFLW" (detector saturation) becomes NA and is tracked, never
#     silently treated as a number.
#   * Well IDs are canonicalised (A01 == A1), so the map always joins.
#
# NEW IN v2 — see the "Plate map" tab:
#   * Click / drag / row / column selection on a live 384- (or 96-) well grid.
#   * Type a sample name, condition, group and control flag -> Apply to selection.
#   * Wells that are not in the reader file are dimmed, so a layout typo is
#     visible immediately instead of silently dropping data.
#   * Paste a block of names straight out of Excel.
#   * Save the map as CSV and load it back for the next plate.
#
# GRAPH OPTIONS FRAME:
#   * A floating window holds everything that changes how a plot looks —
#     size, axis units, tick intervals, log10, text sizes, legend, colours.
#   * Drag it by its title bar, resize it from its bottom-left corner, park it
#     beside the graph. The sidebar checkbox (or its x) hides it.
#   * It follows the open tab: the size sliders always belong to the plot you
#     are looking at, and each plot remembers its own size for the session.
#
# LEGEND AND COLOURS — the "Legend" and "Line colours" sections of that frame:
#   * Legend position, text size, key size, title size and column count.
#   * Order of the lines. Numbers sort as numbers, so a dose series reads
#     0.625, 1.25, 2.5, 5, 10 instead of the 0.625, 1.25, 10, 2.5, 5 that plain
#     alphabetical sorting gives. Also plate order, endpoint order, or a custom
#     list you type. The same order drives the legend, the colours and the
#     panels on the Replicates tab.
#   * A palette (colour-blind safe, viridis family, Brewer, greyscale, or a
#     hue hashed from the name so a line keeps its colour whatever else is
#     plotted), and a click-to-set swatch per line for when the palette is
#     nearly right. A hand-set colour is held against the NAME, so it survives
#     a change of palette, selection or time range.
# =====================================================================

## ---- packages --------------------------------------------------------
need_pkgs <- c("shiny","readxl","dplyr","tidyr","tibble","stringr",
               "purrr","ggplot2","readr","scales","forcats")
missing <- need_pkgs[!vapply(need_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Missing packages. Run this once, then start the app again:\n",
       "install.packages(c(", paste0('"', missing, '"', collapse = ", "), "))")
}
suppressPackageStartupMessages(
  invisible(lapply(need_pkgs, library, character.only = TRUE))
)
options(shiny.maxRequestSize = 200 * 1024^2)   # allow big exports

`%||%` <- function(a, b) if (is.null(a)) b else a

## =====================================================================
## HELPERS
## =====================================================================

# BioTek elapsed-time cell -> seconds. Handles "H:MM:SS", a datetime string
# containing a time, or a fraction-of-day number.
parse_time_to_sec <- function(x) {
  x <- trimws(as.character(x)); out <- rep(NA_real_, length(x))
  # (a) an explicit H:MM:SS anywhere in the cell (covers "13:56:00" and
  #     full datetime strings alike)
  hms <- !is.na(x) & grepl("[0-9]{1,3}:[0-9]{2}:[0-9]{2}", x)
  if (any(hms)) {
    tm <- regmatches(x[which(hms)],
                     regexpr("[0-9]{1,3}:[0-9]{2}:[0-9]{2}", x[which(hms)]))
    p  <- do.call(rbind, strsplit(tm, ":"))
    out[which(hms)] <- as.numeric(p[,1])*3600 + as.numeric(p[,2])*60 + as.numeric(p[,3])
  }
  # (b) otherwise an Excel serial. readxl (col_types="text") returns these as
  #     numbers in SCIENTIFIC notation, e.g. "2.3564814814814813E-2", and plain
  #     "0" for midnight -- so parse numerically rather than by regex.
  #     %% 1 keeps the time-of-day part if a full date-time serial shows up.
  numish <- is.na(out) & !is.na(x) & nzchar(x)
  if (any(numish)) {
    v   <- suppressWarnings(as.numeric(x[which(numish)]))
    ok  <- is.finite(v)
    out[which(numish)[ok]] <- (v[ok] %% 1) * 86400
  }
  out
}

# Undo the mod-24h wrap of the elapsed-time column -> hours.
unwrap_hours <- function(tsec) {
  add <- 0; prev <- NA_real_; o <- numeric(length(tsec))
  for (i in seq_along(tsec)) {
    if (!is.na(prev) && !is.na(tsec[i]) && tsec[i] < prev) add <- add + 86400
    o[i] <- (tsec[i] + add) / 3600
    if (!is.na(tsec[i])) prev <- tsec[i]
  }
  o
}

WELL_RX <- "^[A-P][0-9]{1,2}$"                       # permissive, for header hunting
num <- function(x) suppressWarnings(as.numeric(x))   # "OVRFLW"/"" -> NA

# Canonical well id: "a 01 " / "A01" / "A1" -> "A1"; anything else -> NA.
# Both the reader file and the map go through this, so B2 vs B02 can never
# again silently break the join.
norm_well <- function(x) {
  s   <- toupper(gsub("[^A-Za-z0-9]", "", as.character(x)))
  ok  <- !is.na(s) & grepl("^[A-P](0?[1-9]|1[0-9]|2[0-4])$", s)
  out <- rep(NA_character_, length(s))
  out[ok] <- sub("^([A-P])0*([0-9]+)$", "\\1\\2", s[ok])
  out
}
well_row_i  <- function(w) match(substr(w, 1, 1), LETTERS)
well_col_i  <- function(w) suppressWarnings(as.integer(sub("^[A-Z]", "", w)))
plate_wells <- function(nr, nc) paste0(rep(LETTERS[seq_len(nr)], each = nc),
                                       rep(seq_len(nc), times = nr))

auc_trap <- function(t, y) {
  ok <- !is.na(y) & !is.na(t); t <- t[ok]; y <- y[ok]
  if (length(t) < 2) return(NA_real_)
  o <- order(t); t <- t[o]; y <- y[o]
  sum(diff(t) * (head(y, -1) + tail(y, -1)) / 2)
}

## ---- axis display ----------------------------------------------------
# Time is carried in hours everywhere upstream. These multipliers rescale it
# for the x-axis only: the AUC window, the QC onset times and the CSVs stay in
# hours, so switching a plot to minutes never silently changes a number that
# someone has already written down.
TIME_MULT <- c(s = 3600, min = 60, h = 1, d = 1/24)

# TRUE only for one usable positive number. numericInput returns NA when its
# box is emptied, so NULL / NA / zero all have to fail closed to "automatic".
pos_num <- function(x) !is.null(x) && length(x) == 1L && is.finite(x) && x > 0

# Fixed-width axis breaks, guarded. A tick interval is typed in blind, and an
# interval that is right for a Fold axis (0.5) is catastrophic on a raw signal
# axis spanning 0-50,000: ggplot would try to draw 100,000 ticks and the
# session would hang. Anything asking for more than max_ticks reverts to the
# automatic breaks instead.
width_breaks <- function(w, max_ticks = 60) {
  function(limits) {
    lim <- suppressWarnings(range(limits, na.rm = TRUE))
    if (!all(is.finite(lim))) return(numeric(0))   # panel with no finite data
    if (!pos_num(w) || diff(lim) / w > max_ticks)
      return(scales::extended_breaks()(lim))
    scales::fullseq(lim, w)
  }
}

# Stable colour per label: same name -> same hue for the whole session, and
# near-identical names ("Cmp1"/"Cmp2") land far apart on the wheel.
well_hue <- function(x) {
  vapply(x, function(s) {
    if (is.na(s) || !nzchar(s)) return(NA_real_)
    b <- as.integer(charToRaw(enc2utf8(s)))
    ((sum(b * seq_along(b)) %% 97) * 137.5) %% 360
  }, numeric(1), USE.NAMES = FALSE)
}

## ---- line order ------------------------------------------------------
# A sort key that reads embedded numbers as numbers. Plain sorting puts a dose
# series in the order 0.625, 1.25, 10, 2.5, 5 - right for text, wrong on every
# legend it has ever appeared on. Each label is split into runs of digits and
# non-digits, and the digit runs are zero-padded to a fixed width, so that a
# byte-for-byte comparison of the key is a numeric comparison of the number.
# "Cmp 2" therefore precedes "Cmp 10", and a label with no number in it
# ("W", "DMSO") sorts after every number, which is where a control belongs.
natural_key <- function(x) {
  vapply(as.character(x), function(s) {
    if (is.na(s) || !nzchar(s)) return("")
    tk <- regmatches(s, gregexpr("[0-9]+(\\.[0-9]+)?|[^0-9]+", s))[[1]]
    paste0(vapply(tk, function(t) {
      if (!grepl("^[0-9]", t)) return(tolower(t))
      v <- suppressWarnings(as.numeric(t))
      # a number too big to pad would break the alignment the key relies on
      if (is.finite(v) && abs(v) < 1e12) sprintf("%019.6f", v) else tolower(t)
    }, character(1)), collapse = "\u0001")
  }, character(1), USE.NAMES = FALSE)
}

## ---- line colour -----------------------------------------------------
OKABE_ITO <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#999999")

PALETTES <- c("ggplot2 default"               = "hue",
              "Okabe-Ito (colour-blind safe)" = "okabe",
              "Viridis"                       = "viridis",
              "Plasma"                        = "plasma",
              "Magma"                         = "magma",
              "Cividis"                       = "cividis",
              "Brewer Set1"                   = "set1",
              "Brewer Dark2"                  = "dark2",
              "Brewer Paired"                 = "paired",
              "Brewer Spectral"               = "spectral",
              "Greyscale"                     = "grey",
              "Stable per name"               = "stable")

# Every colour ends up in an <input type=color> swatch, which understands
# "#RRGGBB" and nothing else: viridis hands back "#440154FF" and grey.colors
# can hand back a name, and either would land in the picker as black.
hex7 <- function(x) {
  vapply(as.character(x), function(s) {
    if (is.na(s) || !nzchar(s)) return("#777777")
    if (grepl("^#[0-9A-Fa-f]{6}", s)) return(toupper(substr(s, 1L, 7L)))
    r <- tryCatch(grDevices::col2rgb(s), error = function(e) NULL)
    if (is.null(r)) "#777777" else grDevices::rgb(r[1], r[2], r[3], maxColorValue = 255)
  }, character(1), USE.NAMES = FALSE)
}

# n colours from a named palette. All of these come out of scales / grDevices,
# which are already loaded, so the palette list costs nothing to install.
# Brewer palettes run out (Set1 stops at 9); past that the colours are
# interpolated rather than recycled, so two lines never share one.
# "stable" hashes the label instead of counting along a palette: a line keeps
# its colour whatever else is plotted beside it, and it is the hue its wells
# already carry on the plate map.
pal_fun <- function(name, n, labels = NULL) {
  n <- suppressWarnings(as.integer(n))
  if (!is.finite(n) || n < 1L) return(character(0))
  ramp <- function(v) {
    v <- v[!is.na(v)]
    if (!length(v)) return(rep("#3B6EA5", n))
    if (n <= length(v)) v[seq_len(n)] else grDevices::colorRampPalette(v)(n)
  }
  brew <- function(p, mx) ramp(suppressWarnings(
    scales::brewer_pal(palette = p)(max(3L, min(n, mx)))))
  vir <- function(o) scales::viridis_pal(option = o)(n)
  hex7(switch(
    name %||% "hue",
    okabe    = ramp(OKABE_ITO),
    viridis  = vir("D"),
    plasma   = vir("C"),
    magma    = vir("A"),
    cividis  = vir("E"),
    set1     = brew("Set1",      9L),
    dark2    = brew("Dark2",     8L),
    paired   = brew("Paired",   12L),
    spectral = brew("Spectral", 11L),
    grey     = grDevices::grey.colors(n, start = 0.15, end = 0.72),
    stable   = {
      h <- well_hue(rep_len(as.character(labels %||% seq_len(n)), n))
      h[is.na(h)] <- 0
      grDevices::hcl(h = h, c = 68, l = 55)
    },
    scales::hue_pal()(n)))
}

# Parse every kinetic block on a sheet -> long tibble
# (cycle, time_sec, time_h, well, value, channel, saturated)
parse_reader_sheet <- function(path, sheet) {
  raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                            col_types = "text", .name_repair = "minimal")
  mat <- as.matrix(raw)
  if (nrow(mat) == 0 || ncol(mat) < 3) return(NULL)

  # A block header is any row containing a cell exactly "Time" in the first
  # few columns, with well-like labels to its right.
  hdrs <- list()
  scan_cols <- seq_len(min(4, ncol(mat)))
  for (r in seq_len(nrow(mat))) {
    hit <- which(!is.na(mat[r, scan_cols]) & trimws(mat[r, scan_cols]) == "Time")
    if (!length(hit)) next
    tcol <- hit[1]
    if (tcol >= ncol(mat)) next                 # nothing to the right of Time
    right <- seq.int(tcol + 1L, ncol(mat))
    wmask <- !is.na(mat[r, right]) & grepl(WELL_RX, trimws(mat[r, right]))
    if (sum(wmask) >= 3) hdrs[[length(hdrs) + 1L]] <- list(row = r, tcol = tcol)
  }
  if (!length(hdrs)) return(NULL)

  # Channel label: the "T° Read ..." cell beside Time, else nearest "Read ..."
  # label sitting above the block in column 1.
  channel_for <- function(r, tcol) {
    lab <- if (tcol + 1L <= ncol(mat)) mat[r, tcol + 1L] else NA_character_
    # e.g. "T° Read 3:480,510" -> "Read 3:480,510". Extract from "Read" onward
    # so the degree symbol's encoding never matters.
    if (length(lab) == 1L && !is.na(lab) && grepl("Read", lab))
      return(stringr::str_squish(stringr::str_extract(lab, "Read.*$")))
    if (r > 1L) {                               # guard: no rows above row 1
      for (rr in seq.int(r - 1L, max(1L, r - 8L))) {
        v <- mat[rr, 1]
        if (length(v) == 1L && !is.na(v) && grepl("^Read\\b", trimws(v)))
          return(stringr::str_squish(v))
      }
    }
    paste("Channel @row", r)
  }

  purrr::map_dfr(hdrs, function(h) {
    r <- h$row; tcol <- h$tcol
    ch <- channel_for(r, tcol)
    wcols <- seq.int(tcol + 1L, ncol(mat))
    wname <- trimws(mat[r, wcols])
    keep  <- !is.na(wname) & grepl(WELL_RX, wname)
    wcols <- wcols[keep]; wname <- norm_well(wname[keep])
    ok    <- !is.na(wname) & !duplicated(wname)   # never build duplicate columns
    wcols <- wcols[ok];   wname <- wname[ok]
    if (!length(wcols)) return(NULL)

    rr <- r + 1L; tsec <- c(); rows <- list()
    while (rr <= nrow(mat)) {
      ts <- parse_time_to_sec(mat[rr, tcol])
      if (is.na(ts)) break
      tsec <- c(tsec, ts)
      rows[[length(rows) + 1L]] <- mat[rr, wcols]
      rr <- rr + 1L
    }
    if (!length(rows)) return(NULL)
    bm <- do.call(rbind, rows); colnames(bm) <- wname
    # Unwrap time HERE, per block: each channel is read at its own offset
    # (e.g. fluorescence at 13:56, OD at 16:56 of the same cycle), so a single
    # cycle -> time lookup shared across channels would be wrong.
    tibble::tibble(cycle = seq_len(nrow(bm)), time_sec = tsec,
                   time_h = unwrap_hours(tsec)) |>
      dplyr::bind_cols(tibble::as_tibble(bm)) |>
      tidyr::pivot_longer(-c(cycle, time_sec, time_h), names_to = "well", values_to = "raw") |>
      dplyr::mutate(channel   = ch,
                    saturated = !is.na(raw) & grepl("OVR|SAT", toupper(raw)),
                    value     = num(raw)) |>
      dplyr::select(-raw)
  })
}

## =====================================================================
## PLATE GRID  (rendered as plain HTML; selection handled client-side)
## =====================================================================

plate_html <- function(map, nr, nc, data_wells, sel, field) {
  # force canonical order + completeness so a cell can never render as "NA"
  map <- map[match(plate_wells(nr, nc), map$well), , drop = FALSE]
  map$well <- plate_wells(nr, nc)
  for (cc in c("sample", "condition", "group", "replicate")) {
    v <- map[[cc]]; v[is.na(v)] <- ""; map[[cc]] <- v
  }
  map$is_ctrl[is.na(map$is_ctrl)] <- FALSE

  vals <- switch(field,
                 sample    = map$sample,
                 condition = map$condition,
                 group     = map$group,
                 replicate = map$replicate,
                 well      = map$well,
                 map$sample)
  vals[is.na(vals)] <- ""
  hue  <- well_hue(vals)
  ghue <- well_hue(map$group)
  esc  <- function(z) htmltools::htmlEscape(z, attribute = TRUE)
  dim_unread <- length(data_wells) > 0      # before a file is loaded, dim nothing

  hdr <- paste0(sprintf('<th class="hdr" data-c="%d" title="Select column %d">%d</th>',
                        seq_len(nc), seq_len(nc), seq_len(nc)), collapse = "")
  body <- vapply(seq_len(nr), function(ri) {
    L <- LETTERS[ri]
    tds <- vapply(seq_len(nc), function(ci) {
      k <- (ri - 1L) * nc + ci
      w <- map$well[k]; v <- vals[k]
      cls <- "pw"
      if (dim_unread && !(w %in% data_wells)) cls <- paste(cls, "nodata")
      if (isTRUE(map$is_ctrl[k]))             cls <- paste(cls, "ctrl")
      if (w %in% sel)                         cls <- paste(cls, "sel")
      bg <- if (nzchar(v) && !is.na(hue[k])) sprintf("hsl(%.0f,58%%,86%%)", hue[k]) else "#fbfbfb"
      gb <- if (!is.na(ghue[k])) sprintf("border-left:4px solid hsl(%.0f,60%%,50%%);", ghue[k]) else ""
      tip <- paste0(w,
                    if (nzchar(map$sample[k]))    paste0("  ", map$sample[k]) else "",
                    if (nzchar(map$condition[k])) paste0("  /  ", map$condition[k]) else "",
                    if (nzchar(map$group[k]))     paste0("  /  group: ", map$group[k]) else "",
                    if (nzchar(map$replicate[k])) paste0("  /  rep: ", map$replicate[k]) else "",
                    if (isTRUE(map$is_ctrl[k]))   "  /  control" else "",
                    if (dim_unread && !(w %in% data_wells)) "  /  not read in this file" else "")
      sprintf(paste0('<td class="%s" data-well="%s" data-r="%d" data-c="%d" ',
                     'style="background:%s;%s" title="%s"><div class="lb">%s</div></td>'),
              cls, w, ri, ci, bg, gb, esc(tip), esc(v))
    }, character(1))
    paste0('<tr><th class="hdr" data-r="', ri, '" title="Select row ', L, '">', L,
           '</th>', paste0(tds, collapse = ""), '</tr>')
  }, character(1))

  paste0('<table id="plate" class="plate ', if (nc > 12) "plate384" else "plate96", '">',
         '<tr><th class="hdr" title="Select every well">&#9635;</th>', hdr, '</tr>',
         paste0(body, collapse = ""), '</table>')
}

## ---- look & behaviour of the grid ------------------------------------
plate_css <- HTML("
  .well { padding: 10px; }
  .small-note { color:#666; font-size:11px; margin-top:-2px; }
  h4 { margin-top: 4px; }
  #plateWrap { overflow-x:auto; padding:2px 0 6px 0; }
  table.plate { border-collapse:separate; border-spacing:2px;
                user-select:none; -webkit-user-select:none; -ms-user-select:none; }
  table.plate th.hdr { font-weight:600; color:#666; text-align:center;
                       cursor:pointer; padding:0 1px; border-radius:3px; }
  table.plate th.hdr:hover { color:#111; background:#e8e8e8; }
  table.plate td.pw { border:1px solid #d8d8d8; border-radius:4px; padding:1px;
                      background:#fbfbfb; cursor:cell; }
  table.plate td.pw .lb { overflow:hidden; text-overflow:ellipsis;
                          white-space:nowrap; text-align:center; color:#222; }
  table.plate td.pw.nodata { opacity:.28; border-style:dotted; }
  table.plate td.pw.ctrl   { box-shadow: inset 0 0 0 2px #444; }
  table.plate td.pw.sel    { outline:2px solid #c9302c; outline-offset:-1px; }
  .plate384 th.hdr { font-size:9px; }
  .plate384 td.pw .lb { width:30px; height:21px; line-height:21px; font-size:8px; }
  .plate96  th.hdr { font-size:12px; }
  .plate96  td.pw .lb { width:66px; height:38px; line-height:38px; font-size:11px; }
  @media (prefers-reduced-motion: reduce) { * { transition:none !important; } }
  /* the floating graph-options frame */
  .gfxpanel { z-index:900; background:#fff; border:1px solid #cdcdcd;
              border-radius:6px; box-shadow:0 6px 22px rgba(0,0,0,.20);
              display:flex; flex-direction:column;
              max-height:calc(100vh - 96px); }
  .gfxbar { cursor:move; background:#f1f3f6; border-bottom:1px solid #e2e2e2;
            border-radius:6px 6px 0 0; padding:6px 10px;
            display:flex; align-items:baseline; gap:8px; flex:0 0 auto; }
  .gfxttl { font-weight:600; font-size:13px; color:#333; }
  .gfxsub { font-size:11px; color:#888; flex:1 1 auto;
            overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
  .gfxx { color:#999; font-size:17px; line-height:1; text-decoration:none;
          padding:0 2px; }
  .gfxx:hover, .gfxx:focus { color:#c9302c; text-decoration:none; }
  .gfxbody { padding:6px 12px 12px 12px; overflow-y:auto; flex:1 1 auto; }
  .gfxbody h5 { font-weight:700; font-size:11px; letter-spacing:.04em;
                text-transform:uppercase; color:#7a7a7a;
                margin:12px 0 6px 0; border-bottom:1px solid #eee;
                padding-bottom:3px; }
  .gfxbody h5:first-child { margin-top:2px; }
  .gfxbody .form-group { margin-bottom:9px; }
  .gfxbody .small-note { margin:-4px 0 8px 0; }
  .gfxpanel .ui-resizable-handle { background:transparent; }
  .gfxpanel .ui-resizable-sw { width:14px; height:14px; left:1px; bottom:1px;
                               cursor:sw-resize; }
  /* one row per line: colour swatch + its name */
  #swlist { max-height:230px; overflow-y:auto; margin-bottom:6px;
            padding-right:2px; }
  .swrow { display:flex; align-items:center; gap:7px; margin-bottom:3px; }
  .swrow input.swatch { flex:0 0 auto; width:28px; height:20px; padding:1px;
                        border:1px solid #ccc; border-radius:3px;
                        background:#fff; cursor:pointer; }
  .swlab { font-size:11px; color:#333; overflow:hidden;
           text-overflow:ellipsis; white-space:nowrap; }
")

# Selection lives in the browser while you drag (instant), and is pushed to R
# once on mouse-up. R paints it back on redraw, so the two never disagree.
plate_js <- HTML("
$(function(){
  var dragging = false, anchor = null, base = [];
  function cells(){ return document.querySelectorAll('#plate td.pw'); }
  function key(td){ return td.getAttribute('data-well'); }
  function rc(td){ return [ +td.getAttribute('data-r'), +td.getAttribute('data-c') ]; }
  function snapshot(add){
    base = [];
    if (add) cells().forEach(function(td){ if (td.classList.contains('sel')) base.push(key(td)); });
  }
  function paint(a, b){
    var x = rc(a), y = rc(b);
    var r1 = Math.min(x[0], y[0]), r2 = Math.max(x[0], y[0]);
    var c1 = Math.min(x[1], y[1]), c2 = Math.max(x[1], y[1]);
    cells().forEach(function(td){
      var p = rc(td);
      var on = (p[0] >= r1 && p[0] <= r2 && p[1] >= c1 && p[1] <= c2) ||
               base.indexOf(key(td)) >= 0;
      td.classList.toggle('sel', on);
    });
  }
  function send(){
    var out = [];
    cells().forEach(function(td){ if (td.classList.contains('sel')) out.push(key(td)); });
    Shiny.setInputValue('plate_sel', out, {priority: 'event'});
  }
  $(document).on('mousedown', '#plate td.pw', function(e){
    e.preventDefault();
    dragging = true; anchor = this;
    snapshot(e.shiftKey || e.ctrlKey || e.metaKey);
    paint(anchor, this);
  });
  $(document).on('mouseover', '#plate td.pw', function(){
    if (dragging) paint(anchor, this);
  });
  $(document).on('mouseup', function(){
    if (!dragging) return;
    dragging = false; send();
  });
  $(document).on('click', '#plate th.hdr', function(e){
    var r = this.getAttribute('data-r'), c = this.getAttribute('data-c');
    snapshot(e.shiftKey || e.ctrlKey || e.metaKey);
    cells().forEach(function(td){
      var on = (r === null && c === null) ||
               (r !== null && td.getAttribute('data-r') === r) ||
               (c !== null && td.getAttribute('data-c') === c) ||
               base.indexOf(key(td)) >= 0;
      td.classList.toggle('sel', on);
    });
    send();
  });
  // Ctrl/Cmd + C / V while the plate is on screen. Guarded so it never steals
  // a real text copy or a keystroke aimed at a text box.
  $(document).on('keydown', function(e){
    if (!document.getElementById('plate')) return;
    if (!(e.ctrlKey || e.metaKey)) return;
    var t = (e.target.tagName || '').toUpperCase();
    if (t === 'INPUT' || t === 'TEXTAREA' || t === 'SELECT' || e.target.isContentEditable) return;
    var k = (e.key || '').toLowerCase();
    if (k !== 'c' && k !== 'v') return;
    if (k === 'c' && window.getSelection && String(window.getSelection()) !== '') return;
    e.preventDefault();
    Shiny.setInputValue(k === 'c' ? 'plate_copy_key' : 'plate_paste_key',
                        Date.now(), {priority: 'event'});
  });
  Shiny.addCustomMessageHandler('plateSel', function(m){
    var w = m.wells || [];
    cells().forEach(function(td){ td.classList.toggle('sel', w.indexOf(key(td)) >= 0); });
  });
});
")

## =====================================================================
## GRAPH OPTIONS  —  a floating, draggable frame
## ---------------------------------------------------------------------
# Everything that changes how a plot LOOKS (size, axes, text) lives in one
# frame that floats over the page, instead of being buried in the sidebar
# and in boxes under each plot. You drag it by its title bar, park it beside
# the graph, and watch the graph redraw as you move a slider — no scrolling
# away from the thing you are editing.
#
# Why the controls are all built up-front and merely hidden (conditionalPanel)
# rather than generated per tab (renderUI): a hidden input keeps its value and
# stays bound, so every plot remembers its own size for the whole session and
# switching tabs never resets a slider.
## =====================================================================
PLOT_H0 <- 620L
PLOT_W0 <- 900L

# Size controls for one plot; shown only while its own tab is open.
gfx_size <- function(id, tab, h = PLOT_H0) {
  conditionalPanel(
    sprintf("input.tabs == '%s'", tab),
    sliderInput(paste0("h_", id), "Height (px)", 240, 2000, h, step = 20),
    checkboxInput(paste0("fit_", id), "Fit width to window", TRUE),
    conditionalPanel(
      sprintf("input.fit_%s == false", id),
      sliderInput(paste0("w_", id), "Width (px)", 320, 2400, PLOT_W0, step = 20))
  )
}

gfx_frame <- conditionalPanel(
  "input.gfx_open",
  absolutePanel(
    id = "gfx", class = "gfxpanel", fixed = TRUE, draggable = TRUE,
    top = 76, right = 22, left = "auto", bottom = "auto",
    width = 312, height = "auto",

    div(class = "gfxbar",
        span(class = "gfxttl", "Graph options"),
        span(class = "gfxsub", textOutput("gfx_which", inline = TRUE)),
        actionLink("gfx_hide", HTML("&times;"), class = "gfxx", title = "Hide")),

    div(class = "gfxbody",
        h5("Size"),
        gfx_size("p_time",  "time"),
        gfx_size("p_reps",  "reps"),
        gfx_size("p_auc",   "auc"),
        gfx_size("p_plate", "plate", h = 560L),
        conditionalPanel(
          "['map','qc','data'].indexOf(input.tabs) > -1",
          div(class = "small-note",
              "This tab holds no plot. Open Time course, Replicates, ",
              "AUC ranking or Plate heatmap to size one.")),

        h5("Axes"),
        selectInput("tunit", "Time-axis units",
                    c("Seconds" = "s", "Minutes" = "min",
                      "Hours"   = "h", "Days"    = "d"),
                    selected = "h"),
        div(class = "small-note",
            "Rescales the plotted x-axis only. The AUC window, the QC onsets and the CSVs stay in hours."),
        numericInput("x_int", "Time-axis tick interval (0 = auto)",
                     0, min = 0, step = 1),
        numericInput("y_int", "Value-axis tick interval (0 = auto)",
                     0, min = 0, step = 1),
        checkboxInput("logy", "Log10 y-axis", FALSE),
        div(class = "small-note",
            "Tick intervals apply to the Time course and Replicates plots. An interval that would draw more than 60 ticks, or any value interval while Log10 is on, falls back to auto."),

        h5("Text"),
        sliderInput("sz_text",  "Tick label size (pt)", 5, 20, 9,  step = 1),
        sliderInput("sz_title", "Axis title size (pt)", 6, 24, 11, step = 1),

        h5("Legend"),
        selectInput("leg_pos", "Position",
                    c("Right" = "right", "Bottom" = "bottom", "Top" = "top",
                      "Left"  = "left",  "Hidden" = "none"), selected = "right"),
        selectInput("ser_order", "Order of the lines",
                    c("Numbers in order (0.625, 1.25, 2.5, 5, 10)" = "nat",
                      "Numbers in order, reversed"                 = "nat_rev",
                      "Alphabetical (0.625, 1.25, 10, 2.5, 5)"     = "abc",
                      "Alphabetical, reversed"                     = "abc_rev",
                      "Plate order, A1 first"                      = "plate",
                      "Value at the end, highest first"            = "val",
                      "Custom"                                     = "custom"),
                    selected = "nat"),
        conditionalPanel(
          "input.ser_order == 'custom'",
          textAreaInput("ser_custom", NULL, rows = 4, resize = "vertical",
                        placeholder = "One name per line, in the order you want them."),
          actionLink("ser_fill", "Fill with the order shown now"),
          div(class = "small-note", style = "margin-top:6px",
              "Names you leave out keep their number order and follow on the end.")),
        sliderInput("sz_leg",    "Legend text size (pt)",  4, 20, 9,  step = 1),
        sliderInput("sz_legttl", "Legend title size (pt)", 5, 22, 10, step = 1),
        sliderInput("leg_key",   "Key size (lines)",     0.4,  3, 1,  step = 0.1),
        numericInput("leg_cols", "Legend columns (0 = auto)", 0, min = 0, step = 1),
        div(class = "small-note",
            "The order sets the legend, the colours below and the panel order on ",
            "the Replicates tab. The AUC tab stays ranked by AUC."),

        h5("Line colours"),
        selectInput("palette", "Palette", PALETTES, selected = "hue"),
        div(class = "small-note",
            "The graded palettes - Viridis, Plasma, Magma, Cividis, Greyscale - ",
            "run along the line order above, so a dose series comes out as a ",
            "gradient. Stable per name gives each name the hue its wells already ",
            "carry on the plate map and keeps it whatever else is plotted."),
        uiOutput("ui_series_cols"),
        actionLink("col_reset", "Reset every line to the palette"),
        div(class = "small-note", style = "margin-top:6px",
            "Click a swatch to set one line by hand. A hand-set colour follows ",
            "the name, so it survives a change of palette, selection or time range.")
    )
  )
)

# Two jobs. First, ferry the colour swatches back to R. Second, drag the frame
# by its title bar only, so a slider inside it stays a slider: re-calling
# .draggable() on the panel Shiny has already made draggable just sets the
# option. Both guarded: if jQuery UI is ever absent the frame simply stops
# moving instead of the page erroring out.
gfx_js <- HTML("
$(function(){
  // <input type=color> is not a Shiny input, so collect the swatches by hand
  // and hand R the lot as one name -> colour object. Delegated from document,
  // because the list is re-rendered every time the lines change.
  $(document).on('change', 'input.swatch', function(){
    var out = {};
    $('input.swatch').each(function(){
      out[this.getAttribute('data-series')] = this.value;
    });
    Shiny.setInputValue('series_cols', out, {priority: 'event'});
  });
});
$(function(){
  var p = $('#gfx');
  if (!p.length) return;
  if ($.fn.draggable) p.draggable({ handle: '.gfxbar', containment: 'window',
                                    cancel: '.gfxbody' });
  if ($.fn.resizable) p.resizable({ handles: 'sw, s, w',
                                    minWidth: 250, minHeight: 170 });
});
")

## =====================================================================
## UI
## =====================================================================
ui <- fluidPage(
  tags$head(tags$style(plate_css), tags$script(plate_js), tags$script(gfx_js)),
  titlePanel("Plate Reader Kinetics Explorer"),
  gfx_frame,
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. Data"),
      fileInput("reader", "Reader export (.xlsx)", accept = c(".xlsx", ".xls")),
      uiOutput("ui_sheet"),
      uiOutput("ui_channels"),

      h4("2. Sample names"),
      radioButtons("map_src", NULL,
                   c("Name wells in the app" = "builder",
                     "Use an uploaded map file" = "file",
                     "None - plot well IDs" = "none"),
                   selected = "builder"),
      conditionalPanel(
        "input.map_src == 'builder'",
        div(class = "small-note", "Open the Plate map tab to name wells.")),
      conditionalPanel(
        "input.map_src == 'file'",
        fileInput("map", "Sample map (.xlsx)", accept = c(".xlsx", ".xls")),
        uiOutput("ui_map_sheet"),
        uiOutput("ui_map_cols")),

      h4("3. Processing"),
      selectInput("blank", "Background subtraction",
                  c("None" = "none",
                    "Control-flagged wells" = "ctrl",
                    "Lowest 5% of wells at t0" = "auto"),
                  selected = "none"),
      selectInput("norm", "Plot value",
                  c("Signal, blank-subtracted" = "sig_c",
                    "Signal / Density (per-cell)" = "ratio",
                    "Signal, raw" = "sig_raw",
                    "Fold vs t0" = "fold",
                    "Density (growth)" = "dens"),
                  selected = "sig_c"),
      checkboxInput("show_sem", "Mean \u00b1 SEM ribbon", TRUE),
      checkboxInput("split_reps", "Draw each replicate as its own line", FALSE),
      tags$label("Wells to plot"),
      div(class = "small-note", textOutput("sel_note", inline = TRUE)),
      br(),
      sliderInput("win", "AUC window (h from run start)", 1, 48, 8, step = 1),
      div(class = "small-note",
          "Keep the window before saturation onset - see the QC tab."),
      sliderInput("trange", "Time range shown (h)", 0, 24, c(0, 24), step = 0.5),

      h4("4. Appearance"),
      checkboxInput("gfx_open", "Graph options window", TRUE),
      div(class = "small-note",
          "Size, axes, text, legend and line colours live in that floating ",
          "frame. Drag it by its title bar and park it beside the graph; it ",
          "follows whichever plot tab is open."),
      hr(),
      downloadButton("dl_plot", "Download plot (PNG)"),
      br(), br(),
      downloadButton("dl_summary", "Download time-course (CSV)"),
      br(), br(),
      downloadButton("dl_wells", "Download per-well AUC (CSV)")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",

        tabPanel(
          "Plate map", value = "map",
          br(),
          fluidRow(
            column(3,
                   textInput("s_name", "Sample / compound", ""),
                   textInput("s_cond", "Condition", "")),
            column(3,
                   textInput("s_grp", "Group (one facet per group)", ""),
                   textInput("s_rep", "Replicate", ""),
                   div(class = "small-note",
                       "Labels a well without splitting it off the average.")),
            column(3,
                   checkboxInput("s_ctrl", "Background / control wells", FALSE),
                   br(),
                   tags$label("Auto-number the selection"), br(),
                   actionButton("num_across", "Across, by column"),
                   actionButton("num_down", "Down, by row"),
                   div(class = "small-note", style = "margin-top:8px",
                       "Numbers wells straight away, no Apply needed. ",
                       "Whatever is in the Replicate box becomes the prefix.")),
            column(3,
                   tags$label("\u00a0"), br(),
                   actionButton("apply", "Apply to selection", class = "btn-primary"),
                   actionButton("clear_wells", "Clear selected wells"),
                   br(), br(),
                   actionButton("plot_sel", "Plot just these wells"),
                   div(class = "small-note", style = "margin-top:8px",
                       "A blank box leaves that field as it was."))
          ),
          hr(),
          fluidRow(
            column(6,
                   tags$label("Copy / paste wells"), br(),
                   actionButton("copy_wells",  "Copy selection", class = "btn-sm"),
                   actionButton("paste_wells", "Paste here",     class = "btn-sm"),
                   span(class = "small-note", style = "margin-left:8px",
                        textOutput("clip_note", inline = TRUE)),
                   div(class = "small-note", style = "margin-top:6px",
                       "Copy a well or a block of wells, select where it should land, ",
                       "then paste - the top-left of the new selection is the anchor. ",
                       "Ctrl+C / Ctrl+V do the same. A block pasted into a larger ",
                       "selection is tiled to fill it, so one column of names copies ",
                       "onto the replicate column beside it in two clicks.")),
            column(6,
                   checkboxGroupInput(
                     "paste_fields", "Fields to paste", inline = TRUE,
                     choices  = c("Sample" = "sample", "Condition" = "condition",
                                  "Group" = "group", "Replicate" = "replicate",
                                  "Control" = "is_ctrl"),
                     selected = c("sample", "condition", "group")),
                   div(class = "small-note",
                       "Replicate is off by default: the copy normally goes onto the ",
                       "other replicate of the same sample, which needs a label of its own."))
          ),
          hr(),
          fluidRow(
            column(3, radioButtons("fmt", "Plate", c("384" = "384", "96" = "96"),
                                   selected = "384", inline = TRUE)),
            column(3, selectInput("plate_show", "Label wells with",
                                  c("Sample" = "sample", "Condition" = "condition",
                                    "Group" = "group", "Replicate" = "replicate",
                                    "Well ID" = "well"))),
            column(6, tags$label("Select"), br(),
                   actionButton("sel_all",  "Every well",   class = "btn-sm"),
                   actionButton("sel_data", "Wells in file", class = "btn-sm"),
                   actionButton("sel_none", "Nothing",       class = "btn-sm"))
          ),
          div(class = "small-note", style = "margin-bottom:6px",
              "Click a well, drag across a rectangle, or click a row / column header. ",
              "Shift-click adds to the selection. Dotted, faded wells were not read in this file."),
          div(id = "plateWrap", uiOutput("plate_ui")),
          uiOutput("map_status"),
          hr(),
          fluidRow(
            column(6,
                   h4("Paste a block from Excel"),
                   textAreaInput("paste_txt", NULL, rows = 4, resize = "vertical",
                                 placeholder = "Copy the names out of a spreadsheet and paste here."),
                   actionButton("do_paste", "Fill wells from pasted block"),
                   div(class = "small-note", style = "margin-top:6px",
                       "Names only, no row or column headers. The block lands on the ",
                       "top-left of the current selection, or on A1 if nothing is selected.")),
            column(6,
                   h4("Save / reuse a map"),
                   downloadButton("dl_map", "Download map (CSV)"),
                   br(), br(),
                   fileInput("map_import", "Load a saved map (.csv / .xlsx)",
                             accept = c(".csv", ".xlsx", ".xls")),
                   div(class = "small-note",
                       "Columns are matched by name: well, sample, condition, group, control."))
          )
        ),

        tabPanel("Time course", value = "time",
                 br(), plotOutput("p_time", height = "auto"),
                 br(),
                 tags$details(
                   tags$summary(style = "cursor:pointer; font-size:12px; color:#3b6ea5",
                                "Which wells go into each line?"),
                   br(), tableOutput("t_lines"))),
        tabPanel("Replicates", value = "reps",
                 br(), div(class = "small-note",
                           "One panel per average line, red = the mean itself. ",
                           "Individual wells are coloured by replicate label once you set one."),
                 plotOutput("p_reps", height = "auto")),
        tabPanel("AUC ranking", value = "auc",
                 br(), plotOutput("p_auc", height = "auto"),
                 br(), tableOutput("t_auc")),
        tabPanel("QC", value = "qc",
                 br(),
                 h4("Saturation"), tableOutput("t_sat"),
                 h4("Low-density wells"),
                 div(class = "small-note",
                     "Ratio = Signal/Density is unreliable where density is near zero."),
                 tableOutput("t_lowod"),
                 h4("Replicate spread (CV% of window AUC)"), tableOutput("t_cv")),
        tabPanel("Plate heatmap", value = "plate",
                 br(), div(class = "small-note",
                           "Endpoint of the selected value, by physical well."),
                 plotOutput("p_plate", height = "auto")),
        tabPanel("Data", value = "data", br(), verbatimTextOutput("info"),
                 tableOutput("t_head"))
      )
    )
  )
)

## =====================================================================
## SERVER
## =====================================================================
server <- function(input, output, session) {

  ## ===================================================================
  ## READER FILE
  ## ===================================================================
  sheets <- reactive({
    req(input$reader)
    readxl::excel_sheets(input$reader$datapath)
  })
  output$ui_sheet <- renderUI({
    req(sheets())
    guess <- grep("time|kinet", sheets(), ignore.case = TRUE, value = TRUE)
    selectInput("sheet", "Sheet", choices = sheets(),
                selected = if (length(guess)) guess[1] else sheets()[1])
  })

  parsed_all <- reactive({
    req(input$reader, input$sheet)
    withProgress(message = "Parsing reader export...", value = 0.4, {
      p <- parse_reader_sheet(input$reader$datapath, input$sheet)
    })
    validate(need(!is.null(p) && nrow(p) > 0,
                  "No kinetic block found on this sheet. Pick the sheet that holds the time course."))
    p
  })

  # The reader writes out its full PLANNED schedule: if a 24 h protocol is
  # stopped early, the trailing cycles still carry a timestamp but hold no
  # readings. Those placeholder cycles are dropped, otherwise they stretch the
  # time axis and masquerade as missing/saturated data.
  parsed <- reactive({
    parsed_all() |>
      dplyr::group_by(channel, cycle) |>
      dplyr::filter(!all(is.na(value))) |>
      dplyr::ungroup()
  })

  channels <- reactive(sort(unique(parsed()$channel)))

  output$ui_channels <- renderUI({
    req(channels())
    ch <- channels()
    dens_guess <- grep("600|OD|Abs", ch, ignore.case = TRUE, value = TRUE)
    sig_guess  <- setdiff(ch, dens_guess)
    tagList(
      selectInput("ch_sig", "Signal channel (fluorescence)", ch,
                  selected = if (length(sig_guess)) sig_guess[1] else ch[1]),
      selectInput("ch_dens", "Density channel (OD)", c("(none)", ch),
                  selected = if (length(dens_guess)) dens_guess[1] else "(none)")
    )
  })

  # Wells actually present in the file. Never throws, so the plate map tab
  # works before (and independently of) a successful parse.
  data_wells <- reactive({
    p <- tryCatch(parsed(), error = function(e) NULL)
    if (is.null(p) || !nrow(p)) character(0) else sort(unique(p$well))
  })

  # Sliders are static inputs kept in sync here, rather than renderUI: a
  # renderUI slider is rebuilt (and silently reset) on every upstream change.
  observeEvent(parsed(), {
    r <- range(parsed()$time_h, na.rm = TRUE)
    if (!all(is.finite(r))) return()
    lo <- floor(r[1]); hi <- ceiling(r[2])
    updateSliderInput(session, "trange", min = lo, max = hi,
                      value = c(lo, hi), step = 0.5)
    span <- max(1, ceiling(r[2] - r[1]))
    updateSliderInput(session, "win", max = span,
                      value = min(isolate(input$win) %||% 8, span))
  })

  ## --- wide table: one row per well x cycle ---------------------------
  wide <- reactive({
    p <- parsed()
    req(input$ch_sig)
    validate(need(input$ch_sig %in% p$channel, "Pick a signal channel."))
    sig <- p |> dplyr::filter(channel == input$ch_sig) |>
      dplyr::transmute(cycle, time_h, well, sig = value, sat = saturated) |>
      dplyr::distinct(cycle, well, .keep_all = TRUE)
    # A channel wrapped across several tables carries one Time column per
    # table; without this, two wells of the same cycle can differ by seconds
    # and split into two points when the replicates are averaged.
    sig <- sig |> dplyr::group_by(cycle) |>
      dplyr::mutate(time_h = stats::median(time_h, na.rm = TRUE)) |>
      dplyr::ungroup()
    if (!is.null(input$ch_dens) && input$ch_dens != "(none)") {
      dn <- p |> dplyr::filter(channel == input$ch_dens) |>
        dplyr::transmute(cycle, well, dens = value) |>
        dplyr::distinct(cycle, well, .keep_all = TRUE)
      sig <- sig |> dplyr::left_join(dn, by = c("cycle", "well"))
    } else {
      sig$dens <- NA_real_
    }
    sig
  })

  ## ===================================================================
  ## PLATE MAP BUILDER
  ## ===================================================================
  blank_map <- function(wells) tibble::tibble(
    well = wells, sample = "", condition = "", group = "",
    replicate = "", is_ctrl = FALSE)

  plate_dim <- reactive({
    if ((input$fmt %||% "384") == "96") list(nr = 8L, nc = 12L) else list(nr = 16L, nc = 24L)
  })

  map_state <- reactiveVal(NULL)
  sel       <- reactiveVal(character(0))

  set_sel <- function(w) {
    w <- as.character(w)
    sel(w)
    session$sendCustomMessage("plateSel", list(wells = as.list(w)))
  }

  # Resize the grid without losing what has already been named.
  observeEvent(plate_dim(), {
    dm  <- plate_dim()
    new <- blank_map(plate_wells(dm$nr, dm$nc))
    old <- map_state()
    if (!is.null(old) && nrow(old)) {
      i <- match(new$well, old$well); h <- !is.na(i)
      new$sample[h]    <- old$sample[i[h]]
      new$condition[h] <- old$condition[i[h]]
      new$group[h]     <- old$group[i[h]]
      new$replicate[h] <- old$replicate[i[h]]
      new$is_ctrl[h]   <- old$is_ctrl[i[h]]
    }
    map_state(new)
  })

  # Follow the file: a 96-well export shouldn't be drawn on a 384 grid.
  observeEvent(data_wells(), {
    w <- data_wells(); if (!length(w)) return()
    big <- max(well_col_i(w), na.rm = TRUE) > 12 || max(well_row_i(w), na.rm = TRUE) > 8
    updateRadioButtons(session, "fmt", selected = if (big) "384" else "96")
  }, ignoreInit = TRUE)

  observeEvent(input$plate_sel, {
    s <- unlist(input$plate_sel)
    sel(if (is.null(s)) character(0) else as.character(s))
  }, ignoreNULL = FALSE)

  output$plate_ui <- renderUI({
    m <- map_state(); req(m)
    dm <- plate_dim()
    # sel() is isolated on purpose: the browser already shows the live
    # selection, so redrawing 384 cells on every click would only add lag.
    HTML(plate_html(m, dm$nr, dm$nc, data_wells(), isolate(sel()),
                    input$plate_show %||% "sample"))
  })

  assigned_wells <- reactive({
    m <- map_state(); if (is.null(m)) return(character(0))
    m$well[nzchar(m$sample) | nzchar(m$condition) | nzchar(m$group) |
             nzchar(m$replicate) | m$is_ctrl]
  })

  output$map_status <- renderUI({
    req(map_state())
    dw <- data_wells(); asg <- assigned_wells()
    bits <- c(sprintf("<b>%d</b> wells named", length(asg)),
              sprintf("<b>%d</b> selected", length(sel())))
    if (length(dw)) {
      bits <- c(bits, sprintf("<b>%d</b> of the <b>%d</b> wells in the file are named",
                              sum(dw %in% asg), length(dw)))
      lost <- sum(!(asg %in% dw))
      if (lost) bits <- c(bits, sprintf(
        "<span style='color:#a94442'><b>%d</b> named wells are not in the file</span>", lost))
    } else {
      bits <- c(bits, "<i>load a reader export to see which wells were read</i>")
    }
    HTML(paste0("<div class='small-note' style='margin-top:8px'>",
                paste(bits, collapse = " &nbsp;&middot;&nbsp; "), "</div>"))
  })

  observeEvent(input$apply, {
    s <- sel()
    if (!length(s)) {
      showNotification("Select wells on the plate first - click one, or drag across a block.",
                       type = "warning"); return()
    }
    m <- map_state(); i <- m$well %in% s
    if (nzchar(trimws(input$s_name %||% ""))) m$sample[i]    <- trimws(input$s_name)
    if (nzchar(trimws(input$s_cond %||% ""))) m$condition[i] <- trimws(input$s_cond)
    if (nzchar(trimws(input$s_grp  %||% ""))) m$group[i]     <- trimws(input$s_grp)
    if (nzchar(trimws(input$s_rep  %||% ""))) m$replicate[i] <- trimws(input$s_rep)
    m$is_ctrl[i] <- isTRUE(input$s_ctrl)
    map_state(m)
    showNotification(sprintf("Named %d wells.", sum(i)), type = "message", duration = 2)
  })

  observeEvent(input$clear_wells, {
    s <- sel(); req(length(s))
    m <- map_state(); i <- m$well %in% s
    m$sample[i] <- ""; m$condition[i] <- ""; m$group[i] <- ""
    m$replicate[i] <- ""; m$is_ctrl[i] <- FALSE
    map_state(m)
  })

  # Plate layouts put replicates along a row or down a column, so rank the
  # selected wells on that axis rather than making anyone type Rep 1..3 x 40.
  ## --- well clipboard: copy a named block, paste it onto its replicates ---
  # Stores the copied wells with their offsets from the block's top-left, so a
  # column of names keeps its shape when it lands one column over.
  clip <- reactiveVal(NULL)

  output$clip_note <- renderText({
    b <- clip()
    if (is.null(b) || !nrow(b)) "clipboard empty"
    else sprintf("%d well%s copied", nrow(b), if (nrow(b) == 1) "" else "s")
  })

  do_copy <- function() {
    s <- sel()
    if (!length(s)) {
      showNotification("Select the wells to copy first.", type = "warning")
      return(invisible(NULL))
    }
    m <- map_state(); k <- match(s, m$well); k <- k[!is.na(k)]
    if (!length(k)) return(invisible(NULL))
    b    <- m[k, , drop = FALSE]
    b$dr <- well_row_i(b$well) - min(well_row_i(b$well))
    b$dc <- well_col_i(b$well) - min(well_col_i(b$well))
    clip(b)
    showNotification(sprintf("Copied %d well%s.", nrow(b), if (nrow(b) == 1) "" else "s"),
                     type = "message", duration = 2)
  }

  do_paste <- function() {
    b <- clip()
    if (is.null(b) || !nrow(b)) {
      showNotification("Nothing copied yet - select some named wells and press Copy.",
                       type = "warning"); return(invisible(NULL))
    }
    s <- sel()
    if (!length(s)) {
      showNotification("Select where the block should land - its top-left well is the anchor.",
                       type = "warning"); return(invisible(NULL))
    }
    fl <- intersect(input$paste_fields %||% character(0),
                    c("sample", "condition", "group", "replicate", "is_ctrl"))
    if (!length(fl)) {
      showNotification("No fields are ticked to paste.", type = "warning")
      return(invisible(NULL))
    }
    dm <- plate_dim(); m <- map_state()
    sr <- well_row_i(s); sc <- well_col_i(s)
    r0 <- min(sr); c0 <- min(sc)
    bh <- max(b$dr) + 1L; bw <- max(b$dc) + 1L
    # Excel behaviour: a single anchor well drops one copy; a selection bigger
    # than the copied block is tiled with it.
    nr_t <- max(1L, ceiling((max(sr) - r0 + 1L) / bh))
    nc_t <- max(1L, ceiling((max(sc) - c0 + 1L) / bw))
    n <- 0L; off <- 0L
    for (tr in seq_len(nr_t) - 1L) for (tc in seq_len(nc_t) - 1L) {
      ri   <- r0 + tr * bh + b$dr
      ci   <- c0 + tc * bw + b$dc
      keep <- ri <= dm$nr & ci <= dm$nc
      off  <- off + sum(!keep)
      if (!any(keep)) next
      k  <- match(paste0(LETTERS[ri[keep]], ci[keep]), m$well)
      ok <- !is.na(k)
      if (!any(ok)) next
      src <- which(keep)[ok]
      for (f in fl) m[[f]][k[ok]] <- b[[f]][src]
      n <- n + sum(ok)
    }
    map_state(m)
    showNotification(
      sprintf("Pasted into %d well%s%s.", n, if (n == 1) "" else "s",
              if (off) sprintf("; %d fell off the edge of the plate and were skipped", off) else ""),
      type = "message", duration = 3)
  }

  observeEvent(input$copy_wells,      do_copy())
  observeEvent(input$paste_wells,     do_paste())
  observeEvent(input$plate_copy_key,  do_copy())
  observeEvent(input$plate_paste_key, do_paste())

  number_sel <- function(axis) {
    s <- sel()
    if (!length(s)) {
      showNotification("Select the wells to number first.", type = "warning"); return()
    }
    m   <- map_state()
    pre <- trimws(input$s_rep %||% "")
    idx <- if (axis == "col") well_col_i(s) else well_row_i(s)
    rnk <- match(idx, sort(unique(idx)))
    lab <- if (nzchar(pre)) paste(pre, rnk) else as.character(rnk)
    k   <- match(s, m$well); ok <- !is.na(k)
    m$replicate[k[ok]] <- lab[ok]
    map_state(m)
    showNotification(sprintf("Numbered %d wells 1-%d, %s.", sum(ok), max(rnk),
                             if (axis == "col") "left to right" else "top to bottom"),
                     type = "message", duration = 3)
  }
  observeEvent(input$num_across, number_sel("col"))
  observeEvent(input$num_down,   number_sel("row"))

  observeEvent(input$sel_all,  set_sel(map_state()$well))
  observeEvent(input$sel_none, set_sel(character(0)))
  observeEvent(input$sel_data, set_sel(intersect(map_state()$well, data_wells())))

  observeEvent(input$do_paste, {
    txt <- input$paste_txt %||% ""
    if (!nzchar(trimws(txt))) return()
    ln <- strsplit(txt, "\r?\n")[[1]]
    while (length(ln) && !nzchar(trimws(ln[length(ln)]))) ln <- ln[-length(ln)]
    if (!length(ln)) return()
    sep <- if (any(grepl("\t", ln, fixed = TRUE))) "\t" else ","
    cl  <- strsplit(ln, sep, fixed = TRUE)
    dm  <- plate_dim(); s <- sel()
    r0  <- if (length(s)) min(well_row_i(s)) else 1L
    c0  <- if (length(s)) min(well_col_i(s)) else 1L
    m <- map_state(); n <- 0L
    for (a in seq_along(cl)) {
      v <- cl[[a]]
      for (b in seq_along(v)) {
        ri <- r0 + a - 1L; ci <- c0 + b - 1L
        if (ri > dm$nr || ci > dm$nc) next
        k <- match(paste0(LETTERS[ri], ci), m$well)
        if (is.na(k)) next
        m$sample[k] <- trimws(v[b]); n <- n + 1L
      }
    }
    map_state(m)
    showNotification(sprintf("Filled %d wells from the pasted block.", n),
                     type = "message", duration = 3)
  })

  observeEvent(input$map_import, {
    f <- input$map_import$datapath; nm <- input$map_import$name
    d <- tryCatch(
      if (grepl("\\.csv$", nm, ignore.case = TRUE))
        readr::read_csv(f, show_col_types = FALSE, progress = FALSE)
      else readxl::read_excel(f),
      error = function(e) NULL)
    if (is.null(d) || !nrow(d)) {
      showNotification("That file could not be read.", type = "error"); return()
    }
    cn <- names(d)
    gp <- function(pat) { g <- grep(pat, cn, ignore.case = TRUE, value = TRUE)
                          if (length(g)) g[1] else NA_character_ }
    cw <- gp("^well")
    if (is.na(cw)) {
      showNotification("No 'well' column in that file.", type = "error"); return()
    }
    m <- map_state()
    w <- norm_well(d[[cw]]); k <- match(w, m$well); ok <- !is.na(k)
    if (!any(ok)) {
      showNotification("No well IDs in that file match this plate format.", type = "error"); return()
    }
    setcol <- function(m, col, pat) {
      cc <- gp(pat); if (is.na(cc)) return(m)
      v <- as.character(d[[cc]])[ok]; v[is.na(v)] <- ""
      m[[col]][k[ok]] <- v
      m
    }
    m <- setcol(m, "sample",    "^(sample|compound|name)")
    m <- setcol(m, "condition", "^condition")
    m <- setcol(m, "group",     "^(group|facet|sub_?exp|experiment|plate)")
    m <- setcol(m, "replicate", "^(replicate|rep)$|^rep[._ -]")
    cc <- gp("^(control|blank|is_ctrl)")
    if (!is.na(cc)) {
      v <- d[[cc]]
      v <- if (is.logical(v)) v else toupper(trimws(as.character(v))) %in% c("TRUE","YES","Y","1","W")
      v[is.na(v)] <- FALSE
      m$is_ctrl[k[ok]] <- v[ok]
    }
    map_state(m)
    showNotification(sprintf("Loaded %d wells from %s.", sum(ok), nm),
                     type = "message", duration = 3)
  })

  output$dl_map <- downloadHandler(
    filename = function() paste0("plate_map_", Sys.Date(), ".csv"),
    content  = function(file) {
      m <- map_state() %||% blank_map(character(0))
      readr::write_csv(m[m$well %in% assigned_wells(), ], file)
    })

  ## ===================================================================
  ## UPLOADED MAP FILE  (kept for existing workflows)
  ## ===================================================================
  map_sheets <- reactive({
    req(input$map); readxl::excel_sheets(input$map$datapath)
  })
  output$ui_map_sheet <- renderUI({
    req(map_sheets())
    guess <- grep("map|sample", map_sheets(), ignore.case = TRUE, value = TRUE)
    selectInput("map_sheet", "Map sheet", map_sheets(),
                selected = if (length(guess)) guess[1] else map_sheets()[1])
  })
  map_raw <- reactive({
    req(input$map, input$map_sheet)
    readxl::read_excel(input$map$datapath, sheet = input$map_sheet)
  })
  output$ui_map_cols <- renderUI({
    req(map_raw())
    nm <- names(map_raw())
    pick <- function(pat, allow_none = TRUE) {
      g <- grep(pat, nm, ignore.case = TRUE, value = TRUE)
      if (length(g)) g[1] else if (allow_none) "(none)" else nm[1]
    }
    tagList(
      selectInput("c_well", "Well column", nm, selected = pick("^well", FALSE)),
      selectInput("c_cmp",  "Compound column", c("(none)", nm), selected = pick("compound")),
      selectInput("c_cond", "Condition column", c("(none)", nm), selected = pick("condition")),
      selectInput("c_grp",  "Facet / sub-experiment column", c("(none)", nm),
                  selected = pick("sub_exp|experiment|group|plate")),
      selectInput("c_rep",  "Replicate column", c("(none)", nm), selected = pick("^rep")),
      selectInput("c_ctrl", "Control flag column", c("(none)", nm), selected = pick("control|blank"))
    )
  })

  ## ===================================================================
  ## THE MAP IN USE  ->  join  ->  normalise
  ## ===================================================================
  map_active <- reactive({
    src <- input$map_src %||% "builder"
    if (src == "none") return(NULL)

    if (src == "builder") {
      m <- map_state(); if (is.null(m)) return(NULL)
      m <- m[m$well %in% assigned_wells(), , drop = FALSE]
      if (!nrow(m)) return(NULL)
      return(tibble::tibble(
        well      = m$well,
        compound  = ifelse(nzchar(m$sample), m$sample, m$well),
        condition = m$condition,
        facet     = ifelse(nzchar(m$group), m$group, "All wells"),
        replicate = m$replicate,
        is_ctrl   = m$is_ctrl))
    }

    req(input$map, input$c_well)
    mm <- map_raw(); req(input$c_well %in% names(mm))
    o <- tibble::tibble(well = norm_well(mm[[input$c_well]]))
    o$compound  <- if (!is.null(input$c_cmp)  && input$c_cmp  != "(none)") as.character(mm[[input$c_cmp]])  else o$well
    o$condition <- if (!is.null(input$c_cond) && input$c_cond != "(none)") as.character(mm[[input$c_cond]]) else ""
    o$facet     <- if (!is.null(input$c_grp)  && input$c_grp  != "(none)") as.character(mm[[input$c_grp]])  else "All wells"
    o$replicate <- if (!is.null(input$c_rep)  && input$c_rep  != "(none)") as.character(mm[[input$c_rep]]) else ""
    o$is_ctrl   <- if (!is.null(input$c_ctrl) && input$c_ctrl != "(none)") {
      v <- mm[[input$c_ctrl]]
      if (is.logical(v)) v else toupper(trimws(as.character(v))) %in% c("TRUE","YES","Y","1","W")
    } else FALSE
    o$condition[is.na(o$condition)] <- ""
    o$replicate[is.na(o$replicate)] <- ""
    o$facet[is.na(o$facet)] <- "All wells"
    o$is_ctrl[is.na(o$is_ctrl)] <- FALSE
    o <- o[!is.na(o$well), , drop = FALSE]
    validate(need(nrow(o) > 0,
                  "No usable well IDs in that map file - check the Well column."))
    dplyr::distinct(o, well, .keep_all = TRUE)
  })

  # left_join, not inner_join: an unnamed or mistyped well now shows up as
  # "Unmapped" (and on the heatmap) instead of vanishing without a word.
  dat <- reactive({
    d <- wide()
    m <- map_active()
    if (is.null(m)) {
      d$compound <- d$well; d$condition <- ""; d$facet <- "All wells"
      d$replicate <- "";    d$is_ctrl  <- FALSE;  d$mapped <- FALSE
    } else {
      d <- d |> dplyr::left_join(dplyr::mutate(m, mapped = TRUE), by = "well")
      d$mapped[is.na(d$mapped)] <- FALSE
      u <- !d$mapped
      d$compound[u] <- d$well[u]; d$condition[u] <- ""
      d$facet[u] <- "Unmapped";   d$is_ctrl[u] <- FALSE
      d$replicate[u] <- ""
    }
    d$condition[is.na(d$condition)] <- ""
    d$replicate[is.na(d$replicate)] <- ""
    d$is_ctrl[is.na(d$is_ctrl)] <- FALSE
    # series IS the averaging unit: every well sharing one gets merged into a
    # single mean line. The replicate label deliberately stays out of it,
    # unless you ask to see the replicates drawn separately.
    d$series <- stringr::str_squish(paste(d$compound, d$condition))
    if (isTRUE(input$split_reps))
      d$series <- stringr::str_squish(paste(d$series, d$replicate))
    d
  })

  # Default to control-well blanking as soon as controls exist, but never
  # overwrite a choice the user has already made.
  has_ctrl <- reactive({ m <- map_active(); !is.null(m) && any(m$is_ctrl) })
  observeEvent(has_ctrl(), {
    if (isTRUE(has_ctrl()) && identical(input$blank, "none"))
      updateSelectInput(session, "blank", selected = "ctrl")
  })

  proc <- reactive({
    d <- dat(); req(input$norm)
    blank_mode <- input$blank %||% "none"

    bl <- NULL
    if (blank_mode == "ctrl" && any(d$is_ctrl)) {
      bl <- d |> dplyr::filter(is_ctrl) |> dplyr::group_by(cycle) |>
        dplyr::summarise(bs = mean(sig, na.rm = TRUE),
                         bd = mean(dens, na.rm = TRUE), .groups = "drop")
    } else if (blank_mode == "auto") {
      lo <- d |> dplyr::filter(time_h == min(time_h, na.rm = TRUE)) |>
        dplyr::slice_min(sig, prop = 0.05, na_rm = TRUE) |> dplyr::pull(well) |> unique()
      bl <- d |> dplyr::filter(well %in% lo) |> dplyr::group_by(cycle) |>
        dplyr::summarise(bs = mean(sig, na.rm = TRUE),
                         bd = mean(dens, na.rm = TRUE), .groups = "drop")
    }
    if (is.null(bl)) {
      d$bs <- 0; d$bd <- 0
    } else {
      d <- d |> dplyr::left_join(bl, by = "cycle")
      d$bs[is.na(d$bs)] <- 0; d$bd[is.na(d$bd)] <- 0
    }

    d |>
      dplyr::mutate(sig_c  = sig - bs,
                    dens_c = pmax(dens - bd, 1e-3)) |>
      dplyr::group_by(well) |> dplyr::arrange(time_h, .by_group = TRUE) |>
      dplyr::mutate(t0 = { v <- sig_c[!is.na(sig_c)]; if (length(v)) v[1] else NA_real_ }) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        fold  = ifelse(!is.na(t0) & t0 > 0, sig_c / t0, NA_real_),
        ratio = sig_c / dens_c,
        # AUC window is measured from the first read, not from clock zero:
        # an export whose Time column holds real times of day starts at ~14 h
        # and every AUC came back NA.
        t_rel = time_h - min(time_h, na.rm = TRUE),
        val   = switch(input$norm,
                       sig_c   = sig_c,
                       sig_raw = sig,
                       ratio   = ratio,
                       fold    = fold,
                       dens    = dens,
                       sig_c))
  })

  # Everything except the heatmap works on this: controls removed once they
  # have done their job, then narrowed to whichever wells are being plotted.
  # The plate-map selection IS the filter - no separate control to keep in sync
  # with it. Nothing selected falls back to every named well, and to the whole
  # file while nothing has been named yet.
  use <- reactive({
    d <- proc()
    s <- sel()
    if (identical(input$blank, "ctrl")) d <- d |> dplyr::filter(!is_ctrl)

    if (length(s)) {
      d <- d |> dplyr::filter(well %in% s)
      validate(need(nrow(d) > 0, paste(
        "None of the selected wells can be plotted: either the reader never read",
        "them, or they are flagged as controls and are currently being used for",
        "background subtraction. Clearing the selection (Plate map -> Select ->",
        "Nothing) plots every named well.")))
    } else {
      if (!is.null(map_active())) d <- d |> dplyr::filter(mapped)
      validate(need(nrow(d) > 0, paste(
        "No wells left to plot. Either no well in the file has been named yet",
        "(Plate map tab), or the names sit on wells the reader never read -",
        "the map status line under the grid says which.")))
    }
    d
  })

  # The selection is made on another tab, so spell out here what it is doing to
  # the plots: a stray click on one well silently plotting one line is exactly
  # the kind of thing that should never be invisible.
  output$sel_note <- renderText({
    n <- length(sel())
    if (!n) "Nothing selected on the plate map - plotting every named well."
    else sprintf("%d well%s selected on the plate map - only these are plotted. Select nothing to plot them all.",
                 n, if (n == 1) "" else "s")
  })

  observeEvent(input$plot_sel, {
    if (!length(sel())) {
      showNotification("Select wells on the plate first - click one, or drag across a block.",
                       type = "warning"); return()
    }
    updateTabsetPanel(session, "tabs", selected = "time")
  })

  ## ===================================================================
  ## LINE ORDER AND COLOUR
  ## ===================================================================
  # One ordered vector of names, built once and read by the legend, the colour
  # map and the Replicates panels, so those three can never disagree about
  # which line is which. Built from use() rather than shown(), so that dragging
  # the time range cannot renumber the palette under a line: a series that is
  # off the current range keeps its slot, it just has nothing to draw.
  # Typing a list of names is the one control here that fires on every
  # keystroke, and every keystroke would otherwise redraw every plot.
  ser_custom_d <- debounce(reactive(input$ser_custom), 600)

  ser_lv <- reactive({
    d <- use()
    s <- unique(as.character(d$series)); s <- s[!is.na(s)]
    if (!length(s)) return(character(0))
    ord <- input$ser_order %||% "nat"

    if (identical(ord, "custom")) {
      want <- trimws(strsplit(ser_custom_d() %||% "", "\r?\n")[[1]])
      hit  <- intersect(want[nzchar(want)], s)
      rest <- setdiff(s, hit)
      return(c(hit, rest[order(natural_key(rest), method = "radix")]))
    }

    # Base pass. order(method = "radix") is stable, so where the plate or the
    # endpoint key below ties, the order falls back to this one rather than to
    # whatever order the wells happened to be read in.
    s <- if (ord %in% c("abc", "abc_rev")) s[order(tolower(s), method = "radix")]
         else                              s[order(natural_key(s), method = "radix")]
    if (ord %in% c("abc_rev", "nat_rev")) return(rev(s))

    if (ord %in% c("plate", "val")) {
      k <- if (identical(ord, "plate")) {
        d |> dplyr::group_by(series) |>
          dplyr::summarise(a = suppressWarnings(
            min(well_row_i(well) * 1000 + well_col_i(well), na.rm = TRUE)),
            .groups = "drop")
      } else {
        d |> dplyr::filter(!is.na(val)) |>
          dplyr::group_by(series) |>
          dplyr::filter(time_h == max(time_h, na.rm = TRUE)) |>
          dplyr::summarise(a = -mean(val, na.rm = TRUE), .groups = "drop")
      }
      key <- k$a[match(s, as.character(k$series))]
      key[!is.finite(key)] <- Inf     # a line with nothing to rank on goes last
      s <- s[order(key, method = "radix")]
    }
    s
  })

  # A hand-set colour is held against the name, not the position, so it stays
  # on its own line when the palette, the selection or the order changes.
  eff_cols <- function(base, ov) {
    k <- intersect(names(ov), names(base))
    if (length(k)) base[k] <- ov[k]
    base
  }
  col_override <- reactiveVal(structure(character(0), names = character(0)))
  col_bump     <- reactiveVal(0L)

  pal_cols <- reactive({
    lv <- ser_lv()
    if (!length(lv)) return(structure(character(0), names = character(0)))
    stats::setNames(pal_fun(input$palette %||% "hue", length(lv), lv), lv)
  })
  ser_cols <- reactive(eff_cols(pal_cols(), col_override()))

  output$ui_series_cols <- renderUI({
    col_bump()                                   # redraw after a reset
    lv <- tryCatch(ser_lv(), error = function(e) NULL)
    if (is.null(lv) || !length(lv))
      return(div(class = "small-note",
                 "Load a reader export to list the lines here."))
    # isolate(): the swatch shows its new colour the moment it is clicked, so
    # re-rendering the list from R would do nothing but close the picker.
    cols <- eff_cols(pal_cols(), isolate(col_override()))
    cap  <- 40L
    rows <- lapply(seq_len(min(cap, length(lv))), function(i)
      div(class = "swrow",
          tags$input(type = "color", class = "swatch",
                     `data-series` = lv[i], value = unname(cols[[i]])),
          span(class = "swlab", title = lv[i],
               if (nzchar(lv[i])) lv[i] else "(no name)")))
    tagList(div(id = "swlist", rows),
            if (length(lv) > cap)
              div(class = "small-note",
                  sprintf("%d more lines are not listed. Select the wells you are working on to bring them up here.",
                          length(lv) - cap)))
  })

  # The browser hands back every swatch on screen. An entry counts as an
  # override only while it differs from the palette, so dragging a colour back
  # onto its palette value hands that line back to the palette instead of
  # pinning it to a colour that then ignores the dropdown.
  series_cols_d <- debounce(reactive(input$series_cols), 250)
  observeEvent(series_cols_d(), {
    nv <- unlist(series_cols_d())
    if (is.null(nv) || !length(nv)) return()
    nv <- nv[!is.na(nv) & grepl("^#[0-9A-Fa-f]{6}$", nv)]
    if (!length(nv)) return()
    vals <- toupper(unname(nv)); names(vals) <- names(nv)
    base <- pal_cols(); cur <- col_override()
    k <- intersect(names(vals), names(base))
    if (!length(k)) return()
    same <- unname(vals[k]) == toupper(unname(base[k]))
    cur  <- cur[setdiff(names(cur), k[same])]
    if (any(!same)) cur[k[!same]] <- unname(vals[k[!same]])
    col_override(cur)
  })

  observeEvent(input$col_reset, {
    col_override(structure(character(0), names = character(0)))
    col_bump(col_bump() + 1L)
  })

  observeEvent(input$ser_fill, {
    updateTextAreaInput(session, "ser_custom", value = paste(
      tryCatch(ser_lv(), error = function(e) character(0)), collapse = "\n"))
  })

  shown <- reactive({
    d <- use(); req(input$trange)
    d |> dplyr::filter(time_h >= input$trange[1], time_h <= input$trange[2]) |>
      dplyr::mutate(series = factor(series, levels = ser_lv()),
                    time_x = time_h * tmult())
  })

  vlab <- reactive({
    switch(input$norm,
           sig_c   = "Signal (blank-subtracted)",
           sig_raw = "Signal (raw)",
           ratio   = "Signal / Density",
           fold    = "Fold vs t0",
           dens    = "Density (OD)",
           "Value")
  })

  ## --- axis display ------------------------------------------------------
  # time_h -> the plotted time_x. Filtering and AUC keep using time_h/t_rel.
  tmult <- reactive(unname(TIME_MULT[[input$tunit %||% "h"]]))
  tlab  <- reactive(sprintf("Time (%s)", input$tunit %||% "h"))

  x_scale <- reactive({
    if (pos_num(input$x_int))
      scale_x_continuous(breaks = width_breaks(input$x_int)) else NULL
  })
  # Log and a fixed linear step are mutually exclusive - two y-scales on one
  # plot and ggplot drops the first with a warning - so pick one here rather
  # than adding them at each call site.
  y_scale <- reactive({
    if (isTRUE(input$logy))
      scale_y_log10(labels = scales::label_number())
    else if (pos_num(input$y_int))
      scale_y_continuous(breaks = width_breaks(input$y_int),
                         labels = scales::label_number())
    else NULL
  })

  ## --- summaries --------------------------------------------------------
  summ <- reactive({
    shown() |>
      dplyr::group_by(facet, series, time_h) |>
      dplyr::summarise(mean = mean(val, na.rm = TRUE),
                       sem  = sd(val, na.rm = TRUE) / sqrt(sum(!is.na(val))),
                       n    = sum(!is.na(val)), .groups = "drop") |>
      dplyr::mutate(sem = ifelse(is.finite(sem), sem, 0),
                    time_x = time_h * tmult())
  })

  sat_line <- reactive({
    use() |> dplyr::filter(sat) |> dplyr::group_by(facet) |>
      dplyr::summarise(t_sat = min(time_h, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(t_sat_x = t_sat * tmult())
  })

  kin <- reactive({
    W <- input$win %||% 8
    use() |>
      dplyr::group_by(well, facet, series, compound, condition, replicate) |>
      dplyr::summarise(
        auc       = auc_trap(t_rel[t_rel <= W], val[t_rel <= W]),
        saturated = any(sat),
        t_sat_h   = { s <- sat; if (any(s)) min(time_h[s], na.rm = TRUE) else NA_real_ },
        max_val   = suppressWarnings(max(val, na.rm = TRUE)),
        .groups   = "drop") |>
      dplyr::mutate(max_val = ifelse(is.finite(max_val), max_val, NA_real_))
  })

  ## ===================================================================
  ## GRAPH OPTIONS FRAME
  ## ===================================================================
  # The x closes the frame by driving the same checkbox the sidebar shows, so
  # the two can never disagree about whether the frame is open.
  observeEvent(input$gfx_hide, updateCheckboxInput(session, "gfx_open", value = FALSE))

  # The frame floats free of the tabs, so its title bar has to say what it is
  # currently pointed at.
  output$gfx_which <- renderText({
    switch(input$tabs %||% "time",
           time  = "Time course",
           reps  = "Replicates",
           auc   = "AUC ranking",
           plate = "Plate heatmap",
           "no plot on this tab")
  })

  ## --- plots ------------------------------------------------------------
  # Pixel size of each plot, read from the frame. Returned as functions
  # because that is what renderPlot() takes: it re-draws when the value
  # changes, without the surrounding expression being re-run. An emptied or
  # not-yet-created slider falls back to the default rather than to NA.
  size_h <- function(id, d = PLOT_H0) function() {
    v <- input[[paste0("h_", id)]]; if (pos_num(v)) v else d
  }
  size_w <- function(id, d = PLOT_W0) function() {
    if (!isFALSE(input[[paste0("fit_", id)]])) return("auto")   # NULL -> fit
    v <- input[[paste0("w_", id)]]; if (pos_num(v)) v else d
  }

  # Reactive, so the size sliders reach every plot that uses it. axis.text.x /
  # axis.text.y inherit from axis.text, which is why the AUC plot can set its
  # label angle later without pinning the size back down.
  base_theme <- reactive({
    theme_bw(base_size = 11) +
      theme(panel.grid.minor = element_blank(),
            legend.position = input$leg_pos %||% "right",
            legend.text     = element_text(size = input$sz_leg    %||% 9),
            legend.title    = element_text(size = input$sz_legttl %||% 10),
            legend.key.size = grid::unit(input$leg_key %||% 1, "lines"),
            strip.text = element_text(face = "bold", size = 9),
            axis.text  = element_text(size = input$sz_text  %||% 9),
            axis.title = element_text(size = input$sz_title %||% 11))
  })

  # 0 columns = let ggplot decide.
  leg_ncol <- reactive(if (pos_num(input$leg_cols)) as.integer(input$leg_cols) else NULL)

  p_time_obj <- reactive({
    s  <- summ()
    cm <- ser_cols()
    nc <- leg_ncol()
    p <- ggplot(s, aes(time_x, mean, colour = series, fill = series)) +
      geom_line(linewidth = 0.7) +
      facet_wrap(~ facet, scales = "free_y") +
      # Named values, so a colour is tied to the name of its line rather than
      # to a position in the legend: selecting one more well cannot recolour
      # the line next to it. Same values on both scales, so that the line and
      # its ribbon stay one legend key instead of splitting into two.
      scale_colour_manual(values = cm, na.value = "grey60",
                          guide = guide_legend(ncol = nc)) +
      scale_fill_manual(values = cm, na.value = "grey60",
                        guide = guide_legend(ncol = nc)) +
      labs(x = tlab(), y = vlab(), colour = NULL, fill = NULL) +
      base_theme()
    if (isTRUE(input$show_sem))
      p <- p + geom_ribbon(aes(ymin = mean - sem, ymax = mean + sem),
                           alpha = 0.12, colour = NA)
    if (nrow(sat_line()) && input$norm %in% c("sig_c","sig_raw","ratio","fold"))
      p <- p + geom_vline(data = sat_line(), aes(xintercept = t_sat_x),
                          linetype = "dashed", colour = "grey40", linewidth = 0.4)
    # A downloaded PNG of a subset should say on its face that it is a subset.
    if (length(sel()))
      p <- p + labs(subtitle = sprintf("Plate-map selection only: %d of the %d wells read",
                                       dplyr::n_distinct(shown()$well),
                                       dplyr::n_distinct(proc()$well)))
    # 384 unnamed wells = 384 legend keys and no plot left. Drop the legend and
    # say so rather than rendering something unreadable. Silent if the legend is
    # already off from the frame: no caption to explain a choice they made.
    if (dplyr::n_distinct(s$series) > 30 && !identical(input$leg_pos, "none"))
      p <- p + theme(legend.position = "none") +
        labs(caption = sprintf("%d series - legend hidden. Name wells on the Plate map tab.",
                               dplyr::n_distinct(s$series)))
    p + x_scale() + y_scale()
  })
  output$p_time <- renderPlot(p_time_obj(),
                              width  = size_w("p_time"),
                              height = size_h("p_time"))

  output$p_reps <- renderPlot({
    d <- shown()
    n <- dplyr::n_distinct(d$series)
    validate(need(n <= 60, sprintf(
      "%d separate series - too many panels to read. Give the replicate wells the same name on the Plate map tab.", n)))
    # Panel title carries n, so the red mean can never be mistaken for a
    # single trace or for more wells than it actually has. Panels are factored
    # in the order set in the frame, so they run in the same order as the
    # legend on the Time course tab instead of re-sorting themselves A-Z.
    lab <- d |> dplyr::group_by(series) |>
      dplyr::summarise(panel = sprintf("%s  (n=%d)", as.character(series[1]),
                                       dplyr::n_distinct(well)), .groups = "drop") |>
      dplyr::arrange(series) |>
      dplyr::mutate(panel = forcats::fct_inorder(panel))
    d <- dplyr::left_join(d, lab, by = "series")
    sm <- dplyr::left_join(summ(), lab, by = "series")
    # Replicates are a different thing from series, so they get the palette but
    # not the hand-set colours, which are keyed to series names.
    rl <- unique(d$replicate); rl <- rl[!is.na(rl)]
    rl <- rl[order(natural_key(rl), method = "radix")]
    p <- ggplot(d, aes(time_x, val, group = well))
    p <- if (any(nzchar(d$replicate)))
      p + geom_line(aes(colour = factor(replicate, levels = rl)),
                    alpha = 0.85, linewidth = 0.35) +
        scale_colour_manual(
          values = stats::setNames(pal_fun(input$palette %||% "hue",
                                           length(rl), rl), rl),
          na.value = "grey60", guide = guide_legend(ncol = leg_ncol()))
    else
      p + geom_line(colour = "grey55", alpha = 0.5, linewidth = 0.3)
    p <- p +
      geom_line(data = sm, aes(time_x, mean, group = series),
                inherit.aes = FALSE, colour = "firebrick", linewidth = 0.9) +
      facet_wrap(~ panel, scales = "free_y") +
      labs(x = tlab(), y = vlab(), colour = "Replicate") + base_theme()
    p + x_scale() + y_scale()
  }, width = size_w("p_reps"), height = size_h("p_reps"))

  output$p_auc <- renderPlot({
    k <- kin()
    validate(need(any(!is.na(k$auc)), sprintf(
      "No AUC could be computed in the first %g h - only one time point falls inside the window.",
      input$win)))
    ggplot(k, aes(forcats::fct_reorder(series, auc,
                                       .fun = function(z) mean(z, na.rm = TRUE)), auc)) +
      stat_summary(fun = mean, geom = "col", fill = "#3b6ea5", alpha = 0.85, na.rm = TRUE) +
      stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3, na.rm = TRUE) +
      geom_jitter(width = 0.12, size = 1, alpha = 0.5, na.rm = TRUE) +
      facet_wrap(~ facet, scales = "free") +
      labs(x = NULL, y = sprintf("AUC of %s (first %g h)", vlab(), input$win)) +
      base_theme() +
      theme(axis.text.x = element_text(angle = 55, hjust = 1))
  }, width = size_w("p_auc"), height = size_h("p_auc"))

  output$t_auc <- renderTable({
    kin() |>
      dplyr::group_by(facet, series) |>
      dplyr::summarise(n = dplyr::n(),
                       AUC_mean = mean(auc, na.rm = TRUE),
                       AUC_sd   = sd(auc, na.rm = TRUE),
                       saturated = any(saturated), .groups = "drop") |>
      dplyr::arrange(facet, dplyr::desc(AUC_mean))
  }, digits = 1)

  # Spells out the averaging unit: one row per line on the Time course plot,
  # listing exactly which wells were merged into it.
  output$t_lines <- renderTable({
    shown() |>
      dplyr::distinct(facet, series, well, replicate) |>
      dplyr::group_by(facet, series) |>
      dplyr::summarise(
        wells      = dplyr::n(),
        replicates = paste(ifelse(nzchar(replicate), replicate, "-")[
                             order(well_row_i(well), well_col_i(well))], collapse = ", "),
        well_ids   = paste(well[order(well_row_i(well), well_col_i(well))], collapse = " "),
        .groups    = "drop") |>
      dplyr::arrange(facet, series)
  })

  ## --- QC ---------------------------------------------------------------
  output$t_sat <- renderTable({
    k <- kin()
    if (!any(k$saturated)) return(data.frame(Note = "No saturated readings detected."))
    k |> dplyr::filter(saturated) |>
      dplyr::group_by(facet, series) |>
      dplyr::summarise(wells_saturated = dplyr::n(),
                       earliest_onset_h = min(t_sat_h, na.rm = TRUE),
                       median_onset_h   = median(t_sat_h, na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::arrange(earliest_onset_h)
  }, digits = 2)

  output$t_lowod <- renderTable({
    d <- use()
    if (all(is.na(d$dens))) return(data.frame(Note = "No density channel selected."))
    d |> dplyr::group_by(facet, series) |>
      dplyr::summarise(median_endpoint_density =
                         median(dens[time_h == max(time_h, na.rm = TRUE)], na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::mutate(ratio_reliable = ifelse(median_endpoint_density < 0.05,
                                            "LOW - ratio inflated", "ok")) |>
      dplyr::arrange(median_endpoint_density)
  }, digits = 3)

  output$t_cv <- renderTable({
    kin() |> dplyr::group_by(facet, series) |>
      dplyr::summarise(n = dplyr::n(),
                       CV_pct = 100 * sd(auc, na.rm = TRUE) / mean(auc, na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(CV_pct))
  }, digits = 1)

  ## --- plate heatmap -----------------------------------------------------
  # Built from proc(), so every well the reader read is on it, named or not.
  output$p_plate <- renderPlot({
    d <- proc()
    ep <- d |> dplyr::filter(!is.na(val)) |>
      dplyr::group_by(well) |> dplyr::slice_max(time_h, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::mutate(row = substr(well, 1, 1),
                    col = well_col_i(well))
    validate(need(nrow(ep) > 0, "Nothing to draw yet."))
    ggplot(ep, aes(col, forcats::fct_rev(factor(row)), fill = val)) +
      geom_tile(colour = "white", linewidth = 0.4) +
      scale_fill_viridis_c(option = "magma", labels = scales::label_number()) +
      scale_x_continuous(breaks = sort(unique(ep$col)), position = "top") +
      labs(x = NULL, y = NULL, fill = vlab(), title = "Endpoint by well") +
      coord_equal() + theme_minimal(base_size = 11) +
      theme(axis.text       = element_text(size = input$sz_text   %||% 9),
            legend.position = input$leg_pos %||% "right",
            legend.text     = element_text(size = input$sz_leg    %||% 9),
            legend.title    = element_text(size = input$sz_legttl %||% 10),
            legend.key.size = grid::unit(input$leg_key %||% 1, "lines"))
  }, width = size_w("p_plate"), height = size_h("p_plate", 560L))

  ## --- data tab ----------------------------------------------------------
  output$info <- renderText({
    p <- parsed(); pa <- parsed_all(); d <- dat()
    n_all <- dplyr::n_distinct(pa$cycle); n_use <- dplyr::n_distinct(p$cycle)
    m <- map_active()
    paste0(
      "Channels detected: ", paste(channels(), collapse = " | "), "\n",
      "Cycles with readings: ", n_use, " of ", n_all, " planned",
      if (n_use < n_all)
        sprintf("  (run ended at %.2f h; %d empty trailing cycles dropped)",
                max(p$time_h, na.rm = TRUE), n_all - n_use) else "", "\n",
      "Time: ", sprintf("%.2f", min(p$time_h, na.rm = TRUE)), " - ",
      sprintf("%.2f", max(p$time_h, na.rm = TRUE)), " h\n",
      "Wells in file: ", dplyr::n_distinct(p$well), "\n",
      "Sample map: ",
      if (is.null(m)) "none - plotting well IDs" else
        sprintf("%d wells named, %d of them read in this file, %d read wells left unnamed",
                nrow(m), sum(m$well %in% d$well), dplyr::n_distinct(d$well[!d$mapped])), "\n",
      "True OVRFLW (saturated) readings: ", sum(p$saturated, na.rm = TRUE),
      "   in ", dplyr::n_distinct(p$well[p$saturated]), " well(s)"
    )
  })
  output$t_head <- renderTable(head(use() |>
    dplyr::select(well, series, facet, time_h, sig, dens, val, sat), 25), digits = 2)

  ## --- downloads ---------------------------------------------------------
  output$dl_plot <- downloadHandler(
    filename = function() paste0("timecourse_", Sys.Date(), ".png"),
    content  = function(file) ggsave(file, p_time_obj(), width = 13, height = 8, dpi = 200)
  )
  # Two plain CSVs rather than one zip: utils::zip needs a zip binary on the
  # PATH, which a stock Windows R install does not have.
  # time_x is a plotting convenience; the CSV keeps time_h so an export made
  # with the axis in minutes is still comparable to one made in hours.
  output$dl_summary <- downloadHandler(
    filename = function() paste0("timecourse_summary_", Sys.Date(), ".csv"),
    content  = function(file)
      readr::write_csv(dplyr::select(summ(), -time_x), file)
  )
  output$dl_wells <- downloadHandler(
    filename = function() paste0("kinetics_per_well_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(kin(), file)
  )
}

shinyApp(ui, server)
