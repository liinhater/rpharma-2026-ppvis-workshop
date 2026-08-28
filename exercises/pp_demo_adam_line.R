#---- Pre-setup ----
rm(list = ls())
gc()


library(patientProfilesVis)
library(clinUtils)   # used to access example data for patientProfilesVis
library(dplyr) # included in tidyverse
#' commonly used visualization package; patientProfilesVis outputs ggplot2 objects 
#' that can be further customized
library(ggplot2) # included in tidyverse

#---- R programming ----
# import example data:
data(dataADaMCDISCP01)
# formatted as a list of data.frame (one per domain)
dataADaM <- dataADaMCDISCP01
names(dataADaM)

# and corresponding labels
labelVarsADaM <- attr(dataADaM, "labelVars")
head(labelVarsADaM)

# laboratory parameter
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

adlbcPlots[[1]][[1]]$data #recipe obtained the # for this data
ggplot_recipe(adlbcPlots[[1]][[1]])

gg_line <- adlbcPlots[[1]][[1]]
length(gg_line$layers)

dataADaM$ADLBC %>% distinct(PARCAT1)

#==== Compare ====
# Origin
adlbcPlots <- subjectProfileLinePlot(
  data = dataADaM$ADLBC,
  subsetVar = "PARAMN",
  subsetValue = c(18, 19, 118, 119),
  paramNameVar = "PARAM", 
  paramValueVar = "AVAL",
  paramValueRangeVar = c("A1LO", "A1HI"),
  paramGroupVar = "PARCAT1",
  timeVar = "ADY",
  title = "Laboratory test measurements: actual value",
  labelVars = labelVarsADaM
)

adlbcPlots[[1]][[1]]

# laboratory parameter - custom group and sorted
dataADaM$ADLBC %>% distinct(PARAMN)

adlbc_custom <- dataADaM$ADLBC %>% 
  filter(PARAMN %in% c(18, 19, 118, 119)) %>%
  mutate(
    PARTYPE = if_else(PARAMN < 100, 1, 2),
    PT = "Obs"
  ) %>%
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
  colorPalette = c(Obs = "red"),
  shapeVar = "PT",
  shapePalette = c(Obs = 25)
)

gg_line <- adlbcPlots_custom[[1]][[1]]
inherits(gg_line, "ggplot")

gg_line$data$paramFacetVar # Extract data using to graph

hline_data <- gg_line$data %>%
  dplyr::group_by(paramFacetVar) %>%
  dplyr::summarise(
    span_zero = min(yVar, na.rm = TRUE) <= 0 &
      max(yVar, na.rm = TRUE) >= 0,
    .groups = "drop"
  ) %>%
  dplyr::filter(span_zero) %>%
  dplyr::mutate(yintercept = 0)


gg_line + 
  ggplot2::geom_hline(
    data = hline_data,
    ggplot2::aes(yintercept = yintercept),
    inherit.aes = FALSE,
    linetype = "solid",
    linewidth = 0.6,
    colour = "blue"
  ) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::facet_grid(
    paramFacetVar ~ .,
    scales = "free_y",
    switch = "y",
    axes = "all_x"
  )

#==== single parameter ====
# Generate by pkg directly
adlbcPlots <- subjectProfileLinePlot(
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

adlbcPlots[[1]][[1]]

# Sodium
gg_line_sod <- gg_line$data %>%
  filter(PARAMN == 18)

min(gg_line_sod$ADY)
min(gg_line_sod$A1LO, na.rm = TRUE)
max(gg_line_sod$A1HI, na.rm = TRUE)

ribbon_sod <- gg_line_sod[!(is.na(gg_line_sod$A1LO) & is.na(gg_line_sod$A1HI)), ]

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

# Sodium CHG
gg_line_sodchg <- gg_line$data %>%
  filter(PARAMN == 118 & !is.na(yVar))

min(gg_line_sodchg$yVar, na.rm = TRUE)
max(gg_line_sodchg$yVar, na.rm = TRUE)

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

#Comb
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
