# Plate-reader-analysis
# Plate Reader Kinetics Explorer

A single-file [Shiny](https://shiny.posit.co/) app for exploring kinetic
plate-reader data. Upload a BioTek Synergy kinetic export and get time-courses,
AUC rankings, quality-control tables and a plate heatmap — and name your wells
on a clickable 96- or 384-well map right inside the app. No separate map file,
no code editing.

Everything about the file layout is detected automatically: which sheet holds
the kinetics, where each read block starts, how many channels there are, and
how the time column is encoded. You point it at the raw export and start
looking at data.

---

## Contents

- [Requirements](#requirements)
- [Installation](#installation)
- [Running the app](#running-the-app)
- [What to upload](#what-to-upload)
- [Quick start](#quick-start)
- [The tabs](#the-tabs)
- [Naming wells: the Plate map tab](#naming-wells-the-plate-map-tab)
- [Processing options](#processing-options)
- [The Graph options window](#the-graph-options-window)
- [Exports](#exports)
- [Using a map file instead](#using-a-map-file-instead)
- [How it handles messy exports](#how-it-handles-messy-exports)
- [Troubleshooting](#troubleshooting)

---

## Requirements

- **R** (4.1 or newer recommended — the app uses the native `|>` pipe)
- The following CRAN packages:

  ```
  shiny  readxl  dplyr  tidyr  tibble  stringr
  purrr  ggplot2  readr  scales  forcats
  ```

The app checks for these on startup and, if any are missing, prints the exact
`install.packages(...)` line you need to run.

Uploads up to **200 MB** are allowed, which covers even large multi-channel
384-well exports.

## Installation

1. Save the script into its own folder as **`app.R`**.
2. Install the packages once:

   ```r
   install.packages(c(
     "shiny", "readxl", "dplyr", "tidyr", "tibble", "stringr",
     "purrr", "ggplot2", "readr", "scales", "forcats"
   ))
   ```

## Running the app

**From RStudio:** open `app.R` and click **Run App**.

**From the R console:**

```r
shiny::runApp("path/to/folder")
```

The app opens in your browser (or the RStudio viewer). Nothing is uploaded
anywhere — everything runs locally on your machine.

---

## What to upload

A **BioTek Synergy kinetic export** as `.xlsx` (or `.xls`). You don't need to
clean it up first. The app scans the workbook and works out the structure for
itself:

- **Sheets** are listed from the file; the one whose name looks like a time
  course is pre-selected, and you can switch to any other.
- **Read blocks** are found by locating the `Time` header cell, so the exact
  row and column the block sits in doesn't matter.
- **Channels** — e.g. a fluorescence read (`Read 3:480,510`) and an OD read
  (`Read 4:600`) — are detected and offered as separate **Signal** and
  **Density** channels.
- **Wrapped channels.** When a wide export splits one channel across several
  tables (the 96-column wrap), the pieces are stitched back into one continuous
  trace.

---

## Quick start

1. **Upload** your reader export (sidebar → *1. Data*). Pick the sheet and
   confirm the signal/density channels if the guesses are wrong.
2. **Name your wells** on the **Plate map** tab — click or drag to select,
   type a name, hit **Apply to selection**. (Or skip this and plot raw well
   IDs.)
3. **Choose your processing** (sidebar → *3. Processing*): background
   subtraction, what value to plot, and the AUC window.
4. **Look at the results** on the **Time course**, **Replicates**,
   **AUC ranking**, **QC** and **Plate heatmap** tabs.
5. **Style the plots** in the floating **Graph options** window, then
   **export** a PNG or the underlying CSVs from the sidebar.

---

## The tabs

| Tab | What it shows |
|-----|---------------|
| **Plate map** | The clickable well grid where you name samples, conditions, groups, replicates and controls. |
| **Time course** | Mean line per sample over time, one facet per group, with an optional ± SEM ribbon. A dashed line marks saturation onset. |
| **Replicates** | One panel per sample; the red line is the mean, the thin lines are the individual wells (coloured by replicate label if you set one). |
| **AUC ranking** | Bar chart of area-under-the-curve within your window, ranked, with error bars and per-well points. |
| **QC** | Saturation onset, low-density wells (where a Signal/Density ratio is unreliable), and replicate spread (CV% of the window AUC). |
| **Plate heatmap** | The selected endpoint value laid out by physical well — every well the reader read, named or not. |
| **Data** | A plain-text summary of what was detected (channels, cycles, time span, mapping) and a peek at the processed table. |

---

## Naming wells: the Plate map tab

The plate map is a live 96- or 384-well grid. **Wells that were not read in
your file are dimmed and dotted**, so a layout typo is obvious immediately
instead of silently dropping data.

**Selecting wells**

- Click a single well, or **drag** across a rectangle.
- Click a **row or column header** to select the whole row/column; click the
  top-left corner to select everything.
- **Shift-click** (or Ctrl/Cmd-click) adds to the current selection.

**Labelling a selection**

Type into any of **Sample / compound**, **Condition**, **Group**,
**Replicate**, tick **control** if applicable, then press **Apply to
selection**. A blank box leaves that field untouched.

- **Group** puts a sample into its own facet on the plots without splitting it
  off the average.
- **Replicate** labels a well without changing which wells get averaged
  together — until you turn on *Draw each replicate as its own line*.
- **Control** flags background wells (used by the control-well blanking mode).

**Auto-numbering replicates.** Select a block and press **Across, by column**
or **Down, by row** to number replicates along that axis automatically.
Whatever is in the *Replicate* box becomes the prefix (so `Rep` → `Rep 1`,
`Rep 2`, …).

**Copy / paste wells.** Copy a named well or block, select where it should
land, and paste — the top-left of the new selection is the anchor. `Ctrl+C` /
`Ctrl+V` work too. A small block pasted into a larger selection is **tiled** to
fill it, so copying a column of names onto the replicate column beside it takes
two clicks. Use the *Fields to paste* checkboxes to control what carries over
(replicate is off by default, since the copy usually goes onto a *different*
replicate).

**Paste a block from Excel.** Copy a rectangle of names straight out of a
spreadsheet and paste it into the *Paste a block from Excel* box — it lands on
the top-left of your current selection (or A1 if nothing is selected).

**Save and reuse.** **Download map (CSV)** saves your layout; **Load a saved
map** reads a `.csv` or `.xlsx` back in for the next plate. Columns are matched
by name (`well`, `sample`, `condition`, `group`, `control`, and common
aliases).

---

## Processing options

Found in the sidebar under **3. Processing**.

**Background subtraction**

| Option | Meaning |
|--------|---------|
| None | No subtraction. |
| Control-flagged wells | Subtract the mean of the wells you flagged as controls, per cycle. |
| Lowest 5% of wells at t0 | Auto-pick the dimmest wells at the first time point as a blank. |

(When controls exist, the app switches to control-well blanking for you — but
never overrides a choice you've already made.)

**Plot value**

| Option | Meaning |
|--------|---------|
| Signal, blank-subtracted | The signal channel with background removed (default). |
| Signal / Density (per-cell) | Signal normalised to the density/OD channel. |
| Signal, raw | The signal channel untouched. |
| Fold vs t0 | Signal relative to its own first reading. |
| Density (growth) | The density/OD channel on its own. |

**Other controls**

- **Mean ± SEM ribbon** — shade the standard error around each mean line.
- **Draw each replicate as its own line** — stop averaging replicates together.
- **AUC window (h from run start)** — the integration window for the AUC tab.
  Keep it *before* saturation onset (check the QC tab). The window is measured
  from the first reading, not from clock time, so an export whose timestamps
  are real times of day still integrates correctly.
- **Time range shown (h)** — zoom the plotted time axis without recomputing the
  AUC.

> **Note on units and numbers.** Switching the plotted time axis to minutes,
> seconds or days rescales the x-axis *only*. The AUC window, the QC onset
> times and every exported CSV stay in **hours**, so changing the display never
> silently changes a number you've already written down.

---

## The Graph options window

Everything that changes how a plot **looks** lives in one floating frame
(toggle it with *Graph options window* in the sidebar). Drag it by its title
bar, resize it from the bottom-left corner, and park it next to the graph — it
follows whichever plot tab is open, and each plot remembers its own size for
the session.

- **Size** — height, and either fit-to-window or a fixed width, per plot.
- **Axes** — time units, tick intervals for both axes, log10 y-axis. (An
  interval that would draw more than 60 ticks, or any value interval while
  log10 is on, safely falls back to automatic.)
- **Text** — tick label and axis title sizes.
- **Legend** — position, text/title/key sizes, column count, and the **order of
  the lines**:
  - *Numbers in order* reads embedded numbers as numbers, so a dose series
    reads `0.625, 1.25, 2.5, 5, 10` instead of the `0.625, 1.25, 10, 2.5, 5`
    that plain alphabetical sorting gives.
  - Also alphabetical, plate order, endpoint value, or a custom typed list.
  - The same order drives the legend, the colours, and the panel order on the
    Replicates tab.
- **Line colours** — a palette (Okabe-Ito colour-blind-safe, Viridis family,
  Brewer, greyscale, or a hue hashed from the name so a line keeps its colour
  whatever else is plotted), plus a click-to-set swatch per line. A hand-set
  colour is tied to the **name**, so it survives a change of palette, selection
  or time range.

---

## Exports

From the sidebar:

- **Download plot (PNG)** — the current time-course plot. A subset export is
  labelled as a subset on its face.
- **Download time-course (CSV)** — the summarised mean/SEM table.
- **Download per-well AUC (CSV)** — one row per well with its window AUC,
  saturation flag and endpoint.

From the Plate map tab:

- **Download map (CSV)** — your well layout, for reuse on the next plate.

CSVs are written separately rather than zipped, so no external zip tool is
needed on any platform.

---

## Using a map file instead

If you already keep plate layouts in a spreadsheet, choose **Use an uploaded
map file** in the sidebar (*2. Sample names*) and upload it. You then pick which
columns hold the well ID, compound, condition, group/sub-experiment, replicate
and control flag. Well IDs are canonicalised on both sides, so `B2` and `B02`
always match.

Any well that's named in the map but *wasn't* read in the file (or vice versa)
shows up as **Unmapped** on the plots and heatmap rather than vanishing
silently.

---

## How it handles messy exports

A few things the app takes care of so you don't have to:

- **Time past midnight.** An elapsed-time column that wraps past `24:00:00` is
  unwrapped to real hours — and unwrapped *per channel*, since a fluorescence
  read and an OD read of the same cycle happen at slightly different clock
  times.
- **Detector saturation.** `OVRFLW` (and `SAT`) cells become `NA` and are
  tracked as saturation, never quietly treated as a number. The QC tab reports
  where and when saturation first appears.
- **Early-stopped runs.** If a protocol was stopped before its planned end, the
  trailing cycles still carry a timestamp but no readings. Those empty cycles
  are dropped so they don't stretch the time axis or look like missing data.
- **Well ID canonicalisation.** `A01`, `A1` and `a 01` all resolve to the same
  well, so a `B2` vs `B02` mismatch can never silently break the join between
  your data and your map.

---

## Troubleshooting

**"Missing packages" on startup.** Run the `install.packages(...)` line the app
prints, then start it again.

**"No kinetic block found on this sheet."** You've selected a sheet without a
time course on it. Switch to the sheet that holds the kinetic data (the one
with a `Time` column and well labels).

**A dose series is in the wrong order on the legend.** Set *Order of the lines*
to **Numbers in order** in the Graph options window.

**Nothing plots / "No wells left to plot."** Either no well has been named yet
(name some on the Plate map tab), or your names sit on wells the reader never
read — the status line under the plate grid tells you which. To plot every
named well, clear the selection (*Plate map → Select → Nothing*).

**The plot has hundreds of legend entries and is unreadable.** That's what an
unnamed 384-well plate looks like — every well becomes its own line. Name your
wells so replicates share a name and merge into one mean line.

**A Signal/Density ratio looks inflated.** Check the QC tab's *Low-density
wells* table — the ratio is unreliable where density is near zero.

---

*Built with R and Shiny. Runs entirely on your own machine.*
