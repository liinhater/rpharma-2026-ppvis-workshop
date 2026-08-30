#---- Pre-setup ----
# Shiny app: preview patientProfilesVis ggplot objects for one subject at a time.
# Run from the project root (rpharma-2026-ppvis-workshop/) so paths resolve.
#   setwd("path/to/rpharma-2026-ppvis-workshop")
#   shiny::runApp("exercises/pp_demo_adam_shiny.R")
#
# Plot calls mirror pp_demo_adam_text.R, pp_demo_adam_line.R, and
# pp_demo_adam_interval.R — each returns listPlots[[subject]][[page]].
#
# Pre-setup: same as other exercises — clean session when stepping through
# this script live (avoids leftovers from text / line / interval runs).

rm(list = ls())
gc()

library(patientProfilesVis)
library(clinUtils)
library(dplyr)
library(ggplot2)
library(shiny)
library(cowplot)   # stack multiple ggplot pages vertically (patientProfilesVis dependency)

#---- R programming ----
data(dataADaMCDISCP01)
dataADaM <- dataADaMCDISCP01
labelVarsADaM <- attr(dataADaM, "labelVars")

dataADAE <- dataADaM$ADAE
dataADAE[, "AESEV"] <- factor(
  dataADAE[, "AESEV"],
  levels = c("MILD", "MODERATE", "SEVERE")
)

#---- Run — build plot lists (exercise plot functions) ----

# Text — pp_demo_adam_text.R (Run - THEME, table = FALSE)
text_plots <- subjectProfileTextPlot(
  data = dataADaM$ADSL,
  paramValueVar = c("SEX|AGE", "RACE", "TRT01P"),
  labelVars = labelVarsADaM,
  paramVarSep = " - ",
  table = FALSE
)

# Line — pp_demo_adam_line.R (Run - Starting Point)
line_plots <- subjectProfileLinePlot(
  data = dataADaM$ADLBC,
  paramNameVar = "PARAM",
  paramValueVar = "AVAL",
  paramValueRangeVar = c("A1LO", "A1HI"),
  paramGroupVar = "PARCAT1",
  timeVar = "ADY",
  title = "Laboratory test measurements: actual value",
  labelVars = labelVarsADaM
)

# Interval — pp_demo_adam_interval.R (Run - Starting Point)
interval_plots <- subjectProfileIntervalPlot(
  data = dataADAE,
  paramVar = "AEDECOD",
  timeStartVar = "ASTDY",
  timeEndVar = "AENDY",
  colorVar = "AESEV",
  labelVars = labelVarsADaM,
  timeTrans = getTimeTrans("asinh-neg"),
  title = "Adverse events"
)

subject_choices <- Reduce(
  intersect,
  list(names(text_plots), names(line_plots), names(interval_plots))
)
default_subject <- "01-701-1148"
if (!default_subject %in% subject_choices) {
  default_subject <- subject_choices[1]
}

#---- Shiny UI ----
# selectInput: user picks a subject.
# plotOutput: placeholders; server renderPlot() fills them with ggplot objects.
ui <- fluidPage(
  titlePanel("patientProfilesVis — interactive subject profile preview"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        inputId = "subject",
        label = "Subject (USUBJID)",
        choices = subject_choices,
        selected = default_subject
      )
    ),
    mainPanel(
      width = 9,
      h3("Text (demographics)"),
      plotOutput("plot_text", height = "220px"),
      hr(),
      h3("Line (laboratory)"),
      plotOutput("plot_line", height = "1200px"),
      hr(),
      h3("Interval (adverse events)"),
      plotOutput("plot_interval", height = "400px")
    )
  )
)

#---- Shiny server ----
# listPlots[[subject]] may hold one ggplot or a list of pages — stack with cowplot if > 1.
server <- function(input, output, session) {

  output$plot_text <- renderPlot({
    req(input$subject)
    pages <- text_plots[[input$subject]]
    if (length(pages) == 1) {
      print(pages[[1]])
    } else {
      cowplot::plot_grid(plotlist = pages, ncol = 1, align = "v")
    }
  })

  output$plot_line <- renderPlot({
    req(input$subject)
    pages <- line_plots[[input$subject]]
    if (length(pages) == 1) {
      print(pages[[1]])
    } else {
      cowplot::plot_grid(plotlist = pages, ncol = 1, align = "v")
    }
  })

  output$plot_interval <- renderPlot({
    req(input$subject)
    pages <- interval_plots[[input$subject]]
    if (length(pages) == 1) {
      print(pages[[1]])
    } else {
      cowplot::plot_grid(plotlist = pages, ncol = 1, align = "v")
    }
  })
}

shinyApp(ui = ui, server = server)
