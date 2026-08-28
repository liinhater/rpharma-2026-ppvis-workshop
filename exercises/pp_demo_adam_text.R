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

# demography
adslPlots <- subjectProfileTextPlot(
  data = dataADaM$ADSL,
  paramValueVar = c("SEX|AGE", "RACE", "TRT01P"),
  subjectSubset = c("01-701-1148"), #default: NULL
  labelVars = labelVarsADaM,
  paramVarSep = " - ", 
  table = FALSE,
  colWidth = NULL 
)
ggplot_recipe(adslPlots[[1]][[1]])

adslPlots_tab <- subjectProfileTextPlot(
  data = dataADaM$ADSL,
  paramValueVar = c("SEX", "RACE", "TRT01P"),
  subjectSubset = c("01-701-1148"), #default: NULL
  labelVars = labelVarsADaM,
  table = TRUE, #default: FALSE
  colWidth = c(0.3, 0.2, 0.5) #default: NULL 
)
gg_text <- adslPlots_tab[[1]][[1]] #[[subject]][[page]]
class(gg_text)
length(gg_text$layers)
# 解析成 grob 前的中間結構
#b <- ggplot2::ggplot_build(gg_text)   
#class(b)
#str(b, max.level = 2)
#utils::str(b, max.level = 2)

ggplot_recipe(gg_text)
names(gg_text$layers[[1]])

#avoid using gridExtra 
tab_grob <- gg_text$layers[[1]]$geom_params$grob

#colhead-bg

names(tab_grob$grobs[[4]])
tab_grob$grobs[[4]]$gp

tab_grob$grobs[[4]]$gp <- utils::modifyList(
  tab_grob$grobs[[4]]$gp,
  grid::gpar(col = "grey50")
)

gg_mod <- gg_text
gg_mod$layers[[1]]$geom_params$grob <- tab_grob
gg_mod

names(gg_mod$layers[[1]]$geom_params)
gg_mod$layers[[1]]$geom_params$xmin
gg_mod$layers[[1]]$geom_params$ymin #-Inf
gg_mod$layers[[1]]$geom_params$ymax #Inf

gg_mod$layers[[1]]$geom_params$ymin <- 0.85
gg_mod

patientProfilesVis::

subjectProfileTheme()