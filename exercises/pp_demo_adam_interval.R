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
# ADAE adverse events — each row is one interval (start–end) per AE term.
# Note: subjectProfileEventPlot() handles a single timeVar (geom_point only).
# Intervals with no real duration (start = end, or missing end imputed to start
# via timeImpType = "minimal") are conceptually events, but still draw geom_segment
# plus endpoint symbols — not the same code path as subjectProfileEventPlot().
# ADAE has start/end dates, so interval is the right fit; we skip a separate event
# exercise because the inspect/adapt workflow on the ggplot object is the same.
dataADAE <- dataADaM$ADAE
# Fix severity order for color legend (MILD → MODERATE → SEVERE).
dataADAE[, "AESEV"] <- factor(dataADAE[, "AESEV"], levels = c("MILD", "MODERATE", "SEVERE"))

subject_demo <- "01-701-1148"

# Subset one subject at plot time (subjectSubset), rather than plotting all subjects
# and taking [[1]][[1]]. Either way you get one subject's figure — but if you only
# subset when extracting [[1]][[1]], colorVar legend still reflects severities
# present in THAT subject's rows (e.g. two AESEV levels → two legend entries, not three).
adaePlots <- subjectProfileIntervalPlot(
  data = dataADAE,
  paramVar = "AEDECOD",
  timeStartVar = "ASTDY",
  timeEndVar = "AENDY",
  colorVar = "AESEV",
  labelVars = labelVarsADaM,
  subjectSubset = subject_demo,
  timeTrans = getTimeTrans("asinh-neg"),   # compress negative days; linear for ADY >= 0
  title = "Adverse events"
)

#---- Focus ----
# One subject, one page — adaePlots[[1]][[1]] when subjectSubset has one ID.
gg_int <- adaePlots[[1]][[1]]
gg_int

## Inspect — color legend lists severities present in this subject only.
levels(dataADAE$AESEV)                    # 3 levels defined in ADAE
unique(gg_int$layers[[1]]$data$AESEV)     # levels in this plot / legend (may be fewer)
# Later: colorPalette = ae_sev_palette forces all three levels in the custom plot.

## Inspect — ggplot recipe (layers, facets, labels); see ggplot_recipe.R.
ggplot_recipe(gg_int)

## Inspect — interval plots often have no default $data (waiver); use layer data.
gg_int$data
gg_int$layers[[1]]$data   # layer 1: segment data (geom segment / line)
length(gg_int$layers)

#---- Run - Impute and Legend ----
# Custom intervals: impute missing starts/ends, override one AE segment, data cut.
data_cut_dy <- 7 * 26

adae_custom <- dataADAE %>% 
  filter(USUBJID == subject_demo) %>%
  select(USUBJID, AESEQ, AEDECOD, ASTDY, AENDY, AESEV) %>%
  mutate(
    ASTDY_C = case_when(
      AEDECOD == "LOWER RESPIRATORY TRACT INFECTION" & AESEQ == 3 ~ 10, 
      AEDECOD == "DYSPEPSIA" ~ NA,
      TRUE ~ if_else(ASTDY < 0, NA, ASTDY)
      ),
    AENDY_C = if_else(AEDECOD == "LOWER RESPIRATORY TRACT INFECTION" & AESEQ == 3, 39, AENDY),
    AESEV_C = if_else(AEDECOD == "LOWER RESPIRATORY TRACT INFECTION" & AESEQ == 3, "MILD", AESEV),
    IMP_STDY = 1,
    IMP_ENDY = data_cut_dy
  )

# Extend labelVars for custom columns — use c(..., VAR = "label") not [ ] indexing
# (bracket indexing can produce names like AESEV_C.AESEV).
labelVarsADaM_custom <- c(
  labelVarsADaM,
  ASTDY_C = unname(labelVarsADaM["ASTDY"]),
  AENDY_C = unname(labelVarsADaM["AENDY"]),
  AESEV_C = unname(labelVarsADaM["AESEV"])
)

ae_sev_palette <- c(
  MILD     = "#440154FF",
  MODERATE = "#21918CFF",
  SEVERE   = "#FDE725FF"
)

adaePlots_custom <- subjectProfileIntervalPlot(
  data = adae_custom,
  paramVar = "AEDECOD",
  timeStartVar = "ASTDY_C",
  timeEndVar = "AENDY_C",
  #timeImpType = "data-based",
  timeLimData = adae_custom,
  timeLimStartVar = "IMP_STDY",
  timeLimEndVar = "IMP_ENDY",
  timeLim = c(1, data_cut_dy),
  colorVar = "AESEV_C",
  colorPalette = ae_sev_palette,   # named palette => full legend for all severity levels
  labelVars = labelVarsADaM_custom,
  shapeSize = rel(6),
  #timeTrans = getTimeTrans("asinh-neg"),
  title = "Adverse events"
)

#---- Focus ----
gg_int_custom <- adaePlots_custom[[1]][[1]]
gg_int_custom

## Adapt — x-axis as study weeks; legend styling (covered in depth here vs line demo).
# scale_x_continuous: custom breaks/labels for the data-cut window.
# guides(): separate color (segment) and shape (status symbol) legends.
# theme(legend.position = c(x, y)): npc coords — 0 = left/bottom, 1 = right/top.
gg_int_custom +
  scale_x_continuous(
    breaks = seq(0, data_cut_dy, by = 14),         
    labels = paste("Week", seq(0, data_cut_dy, by = 14)/7, sep = " "),        
    limits = c(1, data_cut_dy),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  guides(
    color = guide_legend(title = "", override.aes = list(linetype = 1, shape = NA)),
    shape = guide_legend(title = "", override.aes = list(linetype = NA))
  ) +
  theme(
    legend.position = c(0.92, 0.15),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.spacing.y = unit(-5, "pt"),
    legend.box = "vertical"
  )
