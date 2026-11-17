# NIU Poster Builder

For the **Psychology Undergraduate Research Conference**. Developed by the **Ortiz Neuroscience Laboratory**, Department of Psychology, Northern Illinois University.

> **Disclaimer:** provided for educational purposes only, as is, without warranty. Students should discuss their poster with their PI or faculty mentor before using or submitting it. Not an official NIU product; follow the conference's poster guidelines and university branding rules. Uploaded files are used only during the session and deleted when it ends.

An R Shiny app that helps students build a 40 × 32 in. conference poster in the NIU template style and download it as an editable PowerPoint file.

## What students can do

- **Header:** title, authors with superscript affiliations (`Alex Rivera^1^`), affiliation lines, and an optional second logo (co-institution or funder) placed next to the NIU logo.
- **Sections:** Introduction, Research Goal, Methods, Figures and Results, Conclusion, Acknowledgements and References. Each can be renamed or hidden.
- **Blocks inside each section,** which can be added, removed and reordered:
  - **Text,** with `**bold**`, `*italic*`, `^superscript^`, `~subscript~` and `- ` bullets
  - **Figure:** any image, including TIFF, with a width slider (20–100% of the panel) and a caption
  - **Figure row:** 2–4 images side by side at equal height, with optional A/B/C labels
  - **Table:** CSV or Excel, styled in NIU red, with adjustable decimals; tiny values such as p-values stay readable
  - **Subheading** inside a panel, in three styles: outlined box, filled red bar, or plain with an underline
  - **Flow chart:** step boxes joined by arrows; wraps into a snake pattern when the panel is narrow
  - **Figure + side text:** image on the left or right, with a dashed, solid or borderless text box beside it
  - **Key-finding callout box**
  - **QR code** linking to a paper, lab site or data
  - **Equation,** typed in LaTeX
- **Custom boxes:** extra panels such as Hypothesis, Future Directions or Contact, added from the Layout tab. Each box is placed above or below any built-in section, gets its own tab, and can hold any block type. In a side-by-side row (Wide layout), a box joins the row.
- **Layouts:** Classic 3-column (the original template), Wide results (2 columns), Methods in the middle (for methods-heavy posters) or Four columns. **Compare layouts for my content** (Layout tab) scores each layout on the actual text and figures and recommends one.
- **Magic arrange** (button next to Download): tries every layout, continuing crowded sections into the next column ("Methods (cont.)") at every possible block, and slightly larger text where there is room. It shows the top three options as mini previews, each with text size, empty space and a list of changes. Nothing changes until you apply one, and **Undo magic** restores the previous arrangement.
- **Continue a section in the next column** by hand from the bottom of any section tab ("Too long for one column?").
- **Figures can be left unnumbered** (logos, decorations) with a checkbox on the figure.
- **Figure growth:** with auto-adjust on, figures in panels that have spare room grow (up to full panel width) instead of leaving empty space. This can be turned off.
- **Auto-adjust (on by default):** column widths shift (within about 25% of the template) so text can be as large as possible and every column is about equally full. Leftover height is spread between blocks instead of collecting at the bottom of panels. On a sparse poster the template widths are kept.
- **Auto-fit:**
  - Text uses the largest size within the chosen range (default 20–28 pt) that fits on the whole poster, so body text is consistent everywhere.
  - Panel heights balance to their content.
  - If content still doesn't fit, figures shrink and the app shows a warning.
- Figures and tables are **numbered automatically** in reading order.
- A **live preview** that matches the download:
  - click a block to jump to its editor
  - drag a block to move it within or between panels; drop it on the middle of another block to swap them
  - drag a figure's red corner handle to resize it
- Any text block can have a dashed or solid border.
- **Save / Load project** as a .zip containing the text and images, so students can come back and keep editing.

## Running locally

```r
install.packages(c("shiny", "bslib", "officer", "flextable", "readxl", "xml2", "jsonlite"))
# optional: TIFF/GIF images, QR codes, equations
install.packages(c("magick", "qrcode", "latex2exp"))
shiny::runApp("poster-builder")
```

`magick`, `qrcode` and `latex2exp` are optional. PNG and JPEG figures work without them. If any package is missing, a banner in the app names it and shows the install command.

## Deploying to Posit Connect Cloud (same as the CV Builder)

1. Push this folder to a GitHub repo, for example `your-username/poster-builder`.
2. In R, from the app folder, run `rsconnect::writeManifest()` and commit the `manifest.json` it creates.
3. In Posit Connect Cloud, choose **Publish → Shiny → from GitHub**, pick the repo and `app.R`.

## Files

| Path | What it does |
|---|---|
| `app.R` | UI and server |
| `R/layout.R` | Layouts, block measuring/stacking, panel balancing, header |
| `R/text.R` | Rich-text parsing and Arial-metric text fitting |
| `R/pptx.R` | Writes the PowerPoint (native shapes and text, officer for images/tables) |
| `R/preview.R` | HTML live preview from the same layout |
| `R/assets.R` | Image/table import, QR codes, equations |
| `R/project.R` | Default blocks (mirror the template), save/load |
| `R/font_metrics.R` | Arial-compatible character widths used to fit text |
| `template/poster_40x32_base.pptx` | Your template with the slide emptied; the gradient and NIU logo stay on the slide master |

## Adding a layout

Add an entry to `LAYOUTS` in `R/layout.R`. Each column has an `x` position, a width `w` and a list of `slots` (section keys, top to bottom). Use `c("a", "b")` to put panels side by side in one slot. `frac` sets the default height split when balancing is turned off.
