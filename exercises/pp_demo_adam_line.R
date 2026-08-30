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

#---- Run - Starting Point ----
# ADLBC laboratory data — default call plots many parameters together.
adlbcPlots <- subjectProfileLinePlot(
  data = dataADaM$ADLBC,
  paramNameVar = "PARAM", 
  paramValueVar = "AVAL",
  paramValueRangeVar = c("A1LO", "A1HI"),
  paramGroupVar = "PARCAT1",
  timeVar = "ADY",
  title = "Laboratory test measurements: actual value",
  labelVars = labelVarsADaM
)

#---- Focus ----
# One subject, one page — start by deconstructing this single ggplot.
gg_line <- adlbcPlots[[1]][[1]]
gg_line

## Inspect — ggplot recipe (layers include geom_ribbon, geom_line, geom_point).
ggplot_recipe(gg_line)

## Inspect — number of layers and grouping variable in ADLBC.
length(gg_line$layers)
dataADaM$ADLBC %>% distinct(PARCAT1)
# paramGroupVar is not "split into separate plot groups" — it mainly controls
# y-axis row order: parameters are sorted by the variable(s) you pass here.
# PARCAT1 is only "CHEM" in this dataset, so the sorting effect is hard to see.

#---- Run - 4 Par Selected ----
# Can we select fewer parameters for a clearer view?
# Narrow to four parameters (actual + change) via subsetVar / subsetValue.
#
# y-axis label line length: formatReport$yLabelWidth (default 30 characters).
# Passed into subjectProfileLinePlot → clinUtils::formatVarForPlotLabel()
# → formatLongLabel(..., width = formatReport$yLabelWidth).
# Use subjectProfileReportFormat(yLabelWidth = ...) to adjust without editing data.
# Default yLabelWidth is 30; here we use 20 for more line breaks on long labels.
adlbcPlots_4par <- subjectProfileLinePlot(
  data = dataADaM$ADLBC,
  subsetVar = "PARAMN",
  subsetValue = c(18, 19, 118, 119),
  paramNameVar = "PARAM", 
  paramValueVar = "AVAL",
  paramValueRangeVar = c("A1LO", "A1HI"),
  paramGroupVar = "PARCAT1",
  timeVar = "ADY",
  title = "Laboratory test measurements: actual value",
  labelVars = labelVarsADaM,
  formatReport = subjectProfileReportFormat(yLabelWidth = 20)
)

#---- Focus ----
adlbcPlots_4par[[1]][[1]]
# Talking point: ggplot may warn —
#   "Removed N rows containing missing values ... ('geom_line()' / 'geom_point()')"
# ADLBC has NA in AVAL / range columns (e.g. SODIUM, K, CL) — expected, not a bug.

## Inspect — PARAMN codes for the subset.
dataADaM$ADLBC %>%
  filter(PARAMN %in% c(18, 19, 118, 119)) %>%
  distinct(PARAMN, PARAM) %>%
  arrange(PARAMN)

## Inspect — Q: How does paramGroupVar organize parameters in the plot?
# A: Not separate plot groups — it sets y-axis ROW ORDER on the same page.
#    Rows sort by paramGroupVar value(s) first, then by paramNameVar (PARAM text).
gg_line_4par <- adlbcPlots_4par[[1]][[1]]
gg_line_4par$data %>%
  distinct(PARAMN, PARAM, PARCAT1, paramFacetVar) %>%
  arrange(paramFacetVar)
# With paramGroupVar = "PARCAT1" (all "CHEM"), order ≈ alphabetical by PARAM —
# e.g. Potassium before Sodium; "change from ..." labels mixed by alpha, not by type.

## Inspect — Q: How are long y-axis labels wrapped automatically?
# A: subjectProfileLinePlot(formatReport = ...) → formatReport$yLabelWidth
#    → clinUtils::formatVarForPlotLabel() → formatLongLabel(..., width = ...).
# Source: patientProfilesVis/R/miscellaneous.R (subjectProfileReportFormat);
#         clinUtils/R/plots-utility.R (formatVarForPlotLabel).
subjectProfileReportFormat()$yLabelWidth   # default: 30
# this run uses yLabelWidth = 20 in formatReport above (more line breaks)

unique(gg_line_4par$data$paramFacetVar)
# Compare raw PARAM text vs wrapped paramFacetVar used on the y-axis:
gg_line_4par$data %>%
  distinct(PARAMN, PARAM, paramFacetVar) %>%
  filter(PARAMN == 118)
long_label <- unique(gg_line_4par$data$PARAM[gg_line_4par$data$PARAMN == 118])
long_label
clinUtils::formatLongLabel(long_label, width = 20)   # matches formatReport above
clinUtils::formatLongLabel(long_label, width = 30)   # compare with package default

#---- Run - Graph Elements ----
# Custom sort: separate "actual" vs "change" params using numeric group keys.
# paramNameVar = text on y-axis; paramGroupVar = sort key (can be numeric).
# formatVarForPlotLabel() applies the same sort + wrap logic with the new keys.
adlbc_custom <- dataADaM$ADLBC %>% 
  filter(PARAMN %in% c(18, 19, 118, 119)) %>%
  mutate(
    PARTYPE = if_else(PARAMN < 100, 1, 2),
    PT = if_else(PARAMN < 100, "AVAL", "CHG")
  ) %>%
  # Drop rows with missing AVAL — e.g. change-from-previous-visit at Baseline (ADY = -9)
  # is NA by design. Without this, ggplot warns:
  #   "Removed N rows containing missing values ... ('geom_line()' / 'geom_point()')"
  filter(!is.na(AVAL))

adlbcPlots_custom <- subjectProfileLinePlot(
  data = adlbc_custom,
  paramNameVar = "PARAM", 
  paramValueVar = "AVAL",
  paramValueRangeVar = c("A1LO", "A1HI"),
  paramGroupVar = c("PARTYPE", "PARAMN"),
  timeVar = "ADY",
  title = "Laboratory test measurements: actual value",
  labelVars = labelVarsADaM,
  colorVar = "PT",
  colorPalette = c("AVAL" = "red", "CHG" = "blue"),
  shapeVar = "PT",
  shapePalette = c("AVAL" = 25, "CHG" = 23)
)

#---- Focus ----
gg_line <- adlbcPlots_custom[[1]][[1]]
gg_line

## Inspect — y-axis row order follows paramGroupVar = c("PARTYPE", "PARAMN").
# Sort keys in the data:
adlbc_custom %>%
  distinct(PARTYPE, PARAMN, PT, PARAM) %>%
  arrange(PARTYPE, PARAMN)
# Row order in the plot:
levels(gg_line$data$paramFacetVar)
# PARTYPE 1 (AVAL: 18, 19) above PARTYPE 2 (CHG: 118, 119).

## Adapt — Q: How can we add reference lines (y = 0)?
# addReferenceLinesProfilePlot() adds VERTICAL time-axis lines only (geom_vline at
# refLines$time / refLinesTimeVar) — not horizontal y = 0 lines for change parameters.
#
# Minimal ggplot2 tweak instead: geom_hline() on the existing gg_line object.
# With faceted panels and scales = "free_y", we must be facet-aware — draw y = 0
# only in panels whose y range spans zero (CHG rows), not in Sodium/Potassium AVAL.
hline_data <- gg_line$data %>%
  dplyr::group_by(paramFacetVar) %>%
  dplyr::summarise(
    span_zero = min(yVar, na.rm = TRUE) <= 0 &
      max(yVar, na.rm = TRUE) >= 0,
    .groups = "drop"
  ) %>%
  dplyr::filter(span_zero) %>%
  dplyr::mutate(yintercept = 0)

## Adapt — Q: Can x-axis tick labels be displayed in every panel?
# Package plot shows x-axis labels on the bottom panel only.
# Re-facet with axes = "all_x" to repeat x tick labels on each row.
gg_line + 
  ggplot2::geom_hline(
    data = hline_data,
    ggplot2::aes(yintercept = yintercept),
    inherit.aes = FALSE,
    linetype = "solid",
    linewidth = 0.6,
    colour = "blue"
  ) +
  # colorVar / shapeVar above would show a PT legend (AVAL vs CHG) by default;
  # legend is covered in the interval exercise — hide it here for a cleaner demo.
  ggplot2::theme(legend.position = "none") +
  ggplot2::facet_grid(
    paramFacetVar ~ .,
    scales = "free_y",
    switch = "y",
    axes = "all_x"   # repeat x-axis tick labels in every panel
  )

#---- Run - Rebuild ----
# Side-by-side Sodium vs Sodium CHG with different y scales and x-axis styling.
# subjectProfileLinePlot() offers yLimFrom, timeLim, xLab, etc., but those apply
# to the whole multi-parameter plot — not per panel. Rebuild each parameter separately
# so breaks/limits and axis labels can differ (e.g. hide x ticks on the top panel).
adlbcPlots_2par <- subjectProfileLinePlot(
  data = dataADaM$ADLBC,
  subsetVar = "PARAMN",
  subsetValue = c(18, 118),
  paramNameVar = "PARAM", 
  paramValueVar = "AVAL",
  paramValueRangeVar = c("A1LO", "A1HI"),
  paramGroupVar = "PARCAT1",
  timeVar = "ADY",
  title = "Laboratory test measurements: actual value",
  labelVars = labelVarsADaM
)

#---- Focus ----
adlbcPlots_2par[[1]][[1]]
gg_line <- adlbcPlots_custom[[1]][[1]]   # reuse processed data from custom run above

## Focus — Sodium actual (PARAMN 18): rebuild one panel with ggplot2.
gg_line_sod <- gg_line$data %>%
  filter(PARAMN == 18)

## Inspect — data range to choose y-axis breaks/limits in scale_y_continuous below.
min(gg_line_sod$ADY)                        # x-axis range (ADY)
min(gg_line_sod$A1LO, na.rm = TRUE)         # reference range low  → ribbon + y scale
max(gg_line_sod$A1HI, na.rm = TRUE)         # reference range high → ribbon + y scale

ribbon_sod <- gg_line_sod[!(is.na(gg_line_sod$A1LO) & is.na(gg_line_sod$A1HI)), ]

## Adapt — geom_ribbon draws the normal-range band (A1LO–A1HI); reuse ppvis theme.
adlbcPlots_sod <- ggplot2::ggplot(gg_line_sod, ggplot2::aes(x = ADY, y = yVar)) +
  ggplot2::geom_ribbon(
    data = ribbon_sod,
    ggplot2::aes(ymin = A1LO, ymax = A1HI),
    fill = "lightgreen", alpha = 0.1
  ) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size = ggplot2::rel(1)) +
  ggplot2::scale_y_continuous(
    name = unique(gg_line_sod$paramFacetVar),
    breaks = seq(130, 150, 5), limits = c(130, 150)
    ) +
  ggplot2::scale_x_continuous(
    name = NULL,
    breaks = seq(-20, 200, 20), limits = c(-20, 200)
  ) +
  gg_line$theme +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(size = 8),
    axis.text.x =  ggplot2::element_blank(),
    axis.ticks.x =  ggplot2::element_blank(), 
    axis.title.x =  ggplot2::element_blank(), 
    axis.title.y = ggplot2::element_text(
      angle = 0,      
      size = 8,
      hjust = 1,
      vjust = 0.5
    )
  )

## Focus — Sodium change (PARAMN 118).
gg_line_sodchg <- gg_line$data %>%
  filter(PARAMN == 118 & !is.na(yVar))

## Inspect — data range to choose y-axis breaks/limits for the change panel.
min(gg_line_sodchg$yVar, na.rm = TRUE)
max(gg_line_sodchg$yVar, na.rm = TRUE)

## Adapt — rebuild second panel; x-axis label on bottom plot only.
# Package-level args (yLimFrom, timeLim, xLab) would apply to all parameters at once;
# per-panel ticks/limits need a separate ggplot + scale_*_continuous, as below.
adlbcPlots_sod_chg <- ggplot2::ggplot(gg_line_sodchg, ggplot2::aes(x = ADY, y = yVar)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size = ggplot2::rel(1)) +
  ggplot2::scale_y_continuous(
    name = unique(gg_line_sodchg$paramFacetVar),
    breaks = seq(-1, 1, 0.5), limits = c(-1, 1)
    ) +
  ggplot2::scale_x_continuous(
    name = "Analysis Relative Day",
    breaks = seq(-20, 200, 20), limits = c(-20, 200)
    ) +
  gg_line$theme +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(size = 8),
    axis.text.x = ggplot2::element_text(size = 8),
    axis.title.x = ggplot2::element_text(size = 8), 
    axis.title.y = ggplot2::element_text(
      angle = 0,      
      size = 8,
      hjust = 1,
      vjust = 0.5
    )
  )

## Adapt — stack two panels side-by-side with cowplot (not facet).
# Use cowplot::plot_grid when each panel is a separate ggplot object.
comb_title <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "Laboratory test measurements: actual value", 
    x = 0, hjust = -0.55, vjust = 1, fontface="bold", size = 12)

comb_fig <- cowplot::plot_grid(
  adlbcPlots_sod,
  adlbcPlots_sod_chg,
  ncol = 1,
  align = "v",
  axis = "lr"
  ) 
  
cowplot::plot_grid(
  comb_title, 
  comb_fig, 
  ncol = 1, 
  rel_heights = c(0.08, 0.92)
  )
