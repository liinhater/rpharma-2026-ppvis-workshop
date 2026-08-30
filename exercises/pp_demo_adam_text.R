#---- Pre-setup ----
rm(list = ls())
gc()


library(patientProfilesVis)
library(clinUtils)   # used to access example data for patientProfilesVis
library(dplyr) # included in tidyverse
#' commonly used visualization package; patientProfilesVis outputs ggplot2 objects 
#' that can be further customized
library(ggplot2) # included in tidyverse

source("exercises/ggplot_recipe.R")

#---- R programming ----
# import example data:
data(dataADaMCDISCP01)
# formatted as a list of data.frame (one per domain)
dataADaM <- dataADaMCDISCP01
names(dataADaM)

# and corresponding labels
labelVarsADaM <- attr(dataADaM, "labelVars")
head(labelVarsADaM)

#---- Theme (patientProfilesVis) ----
# All profile plots start from subjectProfileTheme() (internal, not exported).
# Source: patientProfilesVis/R/miscellaneous.R
#   theme_bw() +
#   theme(panel.grid.major.y = element_line(colour = c("grey80", ...)))
# => alternating grey horizontal grid lines (visible on line/interval plots).
#
# Each plot function adds its own theme(...) on top. For text plots, grids and
# x-axis decoration are removed — see subjectProfileTextPlot.R (~lines 273–284).
#
# subjectProfileTheme() is not exported; call the internal function only for demo:
base_theme <- getFromNamespace("subjectProfileTheme", "patientProfilesVis")
base_theme()

#---- Run - THEME ----
# Generate a text profile plot (table = FALSE).
adslPlots <- subjectProfileTextPlot(
  data = dataADaM$ADSL,
  paramValueVar = c("SEX|AGE", "RACE", "TRT01P"),
  subjectSubset = c("01-701-1148"), #default: NULL
  labelVars = labelVarsADaM,
  paramVarSep = " - ", 
  table = FALSE,
  colWidth = NULL 
)

#---- Focus ----
# One subject [[subject]], one page [[page]] — the ggplot object to explore.
gg_text <- adslPlots[[1]][[1]]
gg_text

## Inspect — ggplot recipe (layers, facets, labels); see ggplot_recipe.R.
ggplot_recipe(gg_text)

## Inspect — package metaData (subject ID, line count for pagination).
names(attributes(gg_text))
attr(gg_text, "metaData")

## Inspect — plot data passed to geom_text (when table = FALSE).
gg_text$data

## Inspect — theme: text plot blanks out grid and x-axis decoration.
# gg$theme is the full merged theme (100+ elements) — too noisy to print whole.
# Check only the elements that subjectProfileTextPlot overrides:
theme_keys <- c(
  "panel.grid.major.x", "panel.grid.minor.x",
  "panel.grid.major.y", "panel.grid.minor.y",
  "axis.text.x", "axis.ticks", "axis.ticks.x"
)
theme_text <- setNames(
  lapply(theme_keys, function(k) gg_text$theme[[k]]),
  theme_keys
)
theme_text
# element_blank() => no grid lines, no x ticks/labels (as intended for text).

#---- Run - Table ----
# Table layout (table = TRUE) — a different output structure to adapt.
adslPlots_tab <- subjectProfileTextPlot(
  data = dataADaM$ADSL,
  paramValueVar = c("SEX", "RACE", "TRT01P"),
  subjectSubset = c("01-701-1148"), #default: NULL
  labelVars = labelVarsADaM,
  table = TRUE, #default: FALSE
  colWidth = c(0.3, 0.2, 0.5) #default: NULL 
)

#---- Focus ----
# Table plot object: one annotation_custom layer holding a tableGrob.
gg_text_tab <- adslPlots_tab[[1]][[1]]

# gridExtra is the usual tool for table theming (ttheme + tableGrob).
# patientProfilesVis already calls it internally; we avoid library(gridExtra)
# here and edit the embedded grob with base grid::gpar() instead.

## Inspect — navigate ggplot layers to find the tableGrob.
names(gg_text_tab$layers)              # table = TRUE => one layer (annotation_custom)
names(gg_text_tab$layers[[1]])         # layer parts: geom, stat, mapping, geom_params, ...
names(gg_text_tab$layers[[1]]$geom_params)  # fixed args; look for "grob"

tab_grob <- gg_text_tab$layers[[1]]$geom_params$grob

## Focus — column-header background rects (layout$name == "colhead-bg").
tab_grob$layout$name
idx_colhead_bg <- which(tab_grob$layout$name == "colhead-bg")
idx_colhead_bg   # e.g. 4, 5, 6 — one rect per header cell

grob_colhead_bg1 <- tab_grob$grobs[[idx_colhead_bg[1]]]

## Inspect — TableGrob print looks 2-D; $grobs is a flat list.
tab_grob
# TableGrob (2 x 3): row 1 = headers, row 2 = data; names: colhead-fg/bg, core-fg/bg.

tab_grob$grobs
# 12 grobs: 3 header texts, 3 header rects, 3 data texts, 3 data rects.

## Inspect — rect grob parts; gp = graphical parameters (grid gpar).
names(grob_colhead_bg1)
grob_colhead_bg1$gp
# gp: col = border colour, fill = background, lwd = border width, ...

## Adapt — header border colour (base R: grid + utils only).
# modifyList merges into existing gp — do not replace gp entirely.
# grid::gpar() names the base-R grid API; not the same as library(gridExtra).
grob_colhead_bg1$gp <- utils::modifyList(
  grob_colhead_bg1$gp,
  grid::gpar(col = "grey50")
)
tab_grob$grobs[[idx_colhead_bg[1]]] <- grob_colhead_bg1

## Adapt — write the modified tableGrob back into the ggplot layer.
gg_mod <- gg_text_tab
gg_mod$layers[[1]]$geom_params$grob <- tab_grob
gg_mod   # preview: header border colour should change

## Inspect — annotation_custom placement in npc (normalized parent coordinates).
# npc: grid coordinate system, 0–1 relative to the panel viewport.
#   0 = bottom/left edge, 1 = top/right edge; -Inf/Inf = stretch to panel edge.
names(gg_mod$layers[[1]]$geom_params)
gg_mod$layers[[1]]$geom_params$xmin   # left edge of table (npc)
gg_mod$layers[[1]]$geom_params$ymin   # default -Inf => stretch to panel bottom
gg_mod$layers[[1]]$geom_params$ymax   # default  Inf => stretch to panel top

## Adapt — nudge table upward by raising ymin (still in npc).
gg_mod$layers[[1]]$geom_params$ymin <- 0.85
gg_mod
