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

# adverse events:
dataADAE <- dataADaM$ADAE
# sort severities
dataADAE[, "AESEV"] <- factor(dataADAE[, "AESEV"], levels = c("MILD", "MODERATE", "SEVERE"))
adaePlots <- subjectProfileIntervalPlot(
  data = dataADAE,
  paramVar = "AEDECOD",
  timeStartVar = "ASTDY",
  timeEndVar = "AENDY",
  colorVar = "AESEV",
  labelVars = labelVarsADaM,
  timeTrans = getTimeTrans("asinh-neg"),
  title = "Adverse events"
)

adaePlots[[1]][[1]]$data #recipe obtained the # for this data
ggplot_recipe(adaePlots[[1]][[1]])


gg_int <- adaePlots[[1]][[1]]
gg_int$data
gg_int_seg_df <- gg_int$layers[[1]]$data

#==== Compare ====
# adverse events - custom AESTDY and AEENDY for same AEDECOD
# Target
data_cut_dy <- 7 * 26

adae_custom <- dataADAE %>% 
  filter(USUBJID == "01-701-1148") %>%
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
  colorPalette = ae_sev_palette,
  labelVars = labelVarsADaM_custom,
  shapeSize = rel(6),
  #timeTrans = getTimeTrans("asinh-neg"),
  title = "Adverse events"
)

gg_int_custom <- adaePlots_custom[[1]][[1]]

# Place legends inside the plot panel using normalized coordinates:
# c(x, y) where x and y range from 0 (left/bottom) to 1 (right/top).
# c(0.92, 0.55) = 92% across from the left, 55% up from the bottom.

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


# Check data for customizing target data
gg_int_custom_seg_df <- gg_int_custom$layers[[1]]$data
gg_int_custom_pt_df <- gg_int_custom$layers[[2]]$data
identical(gg_int_custom_seg_df, gg_int_custom_pt_df)
