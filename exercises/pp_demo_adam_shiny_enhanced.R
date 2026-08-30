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
head(dataADaM$ADSL)

# formatReport: yLabelWidth = wrap width (chars), NOT font size — no font-size knob in ppvis.
text_plots_tab <- subjectProfileTextPlot(
  data = dataADaM$ADSL,
  paramValueVar = c("SEX", "AGE", "RACE", "TRT01P", "TRT01A", "CUMDOSE", "BMIBLGR1", "DCDECOD", "DCREASCD"),
  labelVars = labelVarsADaM,
  paramVarSep = " - ",
  table = TRUE,
  formatReport = subjectProfileReportFormat(yLabelWidth = 40)
)

text_plots <- subjectProfileTextPlot(
  data = dataADaM$ADSL,
  paramValueVar = c("TRTSDT|TRTEDT", "RFSTDTC|RFENDTC", "DISONSDT", "RFENDT"),
  labelVars = labelVarsADaM,
  paramVarSep = " to ",
  table = FALSE,
  formatReport = subjectProfileReportFormat(yLabelWidth = 40),
  title = NULL
)

#---- Adapt text font (ppvis has no font-size argument) ----
# TEXT_SIZE        — pt: table (cex vs base_size 9) + y-axis labels
# TEXT_VALUE_SIZE  — mm: row dates (geom_text); NOT the same unit as TEXT_SIZE
TEXT_SIZE <- 12
TEXT_VALUE_SIZE <- TEXT_SIZE / 3   # ~ppvis default rel(3); tune if dates vs labels mismatch

for (subj in names(text_plots_tab)) {
  for (pg in seq_along(text_plots_tab[[subj]])) {
    gg <- text_plots_tab[[subj]][[pg]]
    g <- gg$layers[[1]]$geom_params$grob
    idx <- which(g$layout$name %in% c("colhead-fg", "core-fg"))
    for (i in idx) {
      g$grobs[[i]]$gp <- utils::modifyList(
        g$grobs[[i]]$gp,
        grid::gpar(cex = TEXT_SIZE / 9)
      )
    }
    gg$layers[[1]]$geom_params$grob <- g
    text_plots_tab[[subj]][[pg]] <- gg
  }
}

for (subj in names(text_plots)) {
  for (pg in seq_along(text_plots[[subj]])) {
    gg <- text_plots[[subj]][[pg]]
    gg <- gg +
      theme(
        axis.text.y.left = element_text(
          size = TEXT_SIZE, face = "bold", colour = "black"
        )
      )
    gg$layers[[1]]$aes_params$size <- TEXT_VALUE_SIZE
    gg$layers[[1]]$aes_params$colour <- "black"
    text_plots[[subj]][[pg]] <- gg
  }
}

#---- Preview — confirm combined text output (before Shiny) ----
# Step through this block interactively to check the stacked result:
#   table (text_plots_tab) + y-axis rows (text_plots), no panel border.
# Uses first subject [[1]][[1]] only; Shiny repeats the same steps per input$subject.
gg_text_tab <- text_plots_tab[[1]][[1]] +
  theme(
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

gg_text_row <- text_plots[[1]][[1]] +
  theme(
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.line = element_blank()
  )

cowplot::plot_grid(
  gg_text_tab,
  gg_text_row,
  ncol = 1,
  rel_heights = c(0.5, 0.5)
)


#---- Line — data prep + parameter menu (pp_demo_adam_line.R) ----
# CDISC Pilot ADLBC has two PARAM sets per analyte:
#   SODIUM  (PARAMN  18) — actual value in mmol/L; A1LO/A1HI = normal range.
#   _SODIUM (PARAMN 118) — separate derived PARAM (PARAMCD with leading "_").
#
# PT = "CHG" below is workshop shorthand for PARAMN >= 100 rows — NOT ADaM CHG
# (CHG = AVAL - BASE on the same row). _SODIUM AVAL is already stored on its own row.
#
# PARAM label "change from previous visit, relative to normal range" means:
#   1) previous visit = last non-missing visit for the *actual* analyte (SODIUM), and
#   2) relative to normal range = change in normalized position within A1LO–A1HI:
#        pos = (AVAL - (A1LO + A1HI) / 2) / ((A1HI - A1LO) / 2)
#        _SODIUM AVAL ≈ pos(current visit) - pos(previous visit)
#      So 141 - 139 = 2 mmol/L becomes ~0.3 (not 2): a unitless shift in half-range units.
#   Verify: filter(PARAMCD %in% c("SODIUM", "_SODIUM")) %>% arrange(ADY) for one subject.
#
# TESTN = same lab test (AVAL PARAMN or CHG PARAMN - 100); PT = AVAL | CHG.
# paramGroupVar = c("TESTN", "PT") keeps each test's AVAL row next to its CHG row.
# ADY_PLOT: map pre-baseline visits (ADY < 0) to 0; x tick at 0 labeled "Baseline".
adlbc_line <- dataADaM$ADLBC %>%
  mutate(
    TESTN = if_else(PARAMN < 100, PARAMN, PARAMN - 100),
    PT = if_else(PARAMN < 100, "AVAL", "CHG"),
    ADY_PLOT = if_else(ADY < 0, 0, ADY),
    # CHG rows (_SODIUM etc.): AVAL is unitless; A1LO/A1HI are still mmol/L ref range.
    # With yLimFrom = "all", free_y scale spans ~136–145 while points sit near 0 → geom_point warning.
    A1LO = if_else(PARAMN >= 100, NA_real_, A1LO),
    A1HI = if_else(PARAMN >= 100, NA_real_, A1HI)
  ) %>%
  filter(!is.na(AVAL))

labelVarsADaM <- c(
  labelVarsADaM,
  ADY_PLOT = unname(labelVarsADaM["ADY"])
)

line_param_catalog <- adlbc_line %>%
  distinct(PARAMN, PARAM, TESTN, PT) %>%
  mutate(PT = factor(PT, levels = c("AVAL", "CHG"))) %>%
  arrange(TESTN, PT)

# Named vector for checkboxGroupInput: value = PARAMN, label = "PARAM  (AVAL|CHG)".
line_param_choices <- setNames(
  line_param_catalog$PARAMN,
  paste0(line_param_catalog$PARAM, "  (", line_param_catalog$PT, ")")
)
line_param_all <- as.character(unname(line_param_choices))

default_line_params <- line_param_catalog %>%
  filter(TESTN == min(TESTN)) %>%
  pull(PARAMN) %>%
  as.character()

default_subject <- "01-701-1148"

# Interval — pp_demo_adam_interval.R (Run - Starting Point)
#
# Missing ASTDY and/or AENDY: ppvis imputes one side from the other and draws
# endpoint symbols (geom_point). Rows that cannot be placed on the time axis
# (e.g. both missing → label only) may be dropped at draw time — ggplot warns:
#   "Removed N rows ... ('geom_point()')"
# That warning is subject-specific (e.g. 01-701-1148 vs 01-704-1445).
# The plot footnote documents the same rule (see caption under the interval panel):
#   missing start → shown at end; missing end → shown at start;
#   both missing → label only.
# The footnote is always drawn by subjectProfileIntervalPlot(), even when the
# subject has no missing start/end and no console warning.
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
  list(
    names(text_plots_tab),
    names(text_plots),
    unique(adlbc_line$USUBJID),
    names(interval_plots)
  )
)
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
      ),
      checkboxInput("line_select_all", "Select all", value = FALSE),
      checkboxInput("line_deselect_all", "Deselect all", value = FALSE),
      checkboxGroupInput(
        inputId = "line_params",
        label = "Line parameters (AVAL / CHG pairs adjacent)",
        choices = line_param_choices,
        selected = default_line_params
      )
    ),
    mainPanel(
      width = 9,
      h3("Text (demographics)"),
      plotOutput("plot_text", height = "240px"),
      hr(),
      h3("Line (laboratory)"),
      uiOutput("plot_line_ui"),
      hr(),
      h3("Interval (adverse events)"),
      plotOutput("plot_interval", height = "480px")
    )
  )
)

#---- Shiny server ----
# listPlots[[subject]] may hold one ggplot or a list of pages — stack with cowplot if > 1.
server <- function(input, output, session) {

  observeEvent(input$line_select_all, {
    if (isTRUE(input$line_select_all)) {
      updateCheckboxGroupInput(session, "line_params", selected = line_param_all)
      updateCheckboxInput(session, "line_select_all", value = FALSE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$line_deselect_all, {
    if (isTRUE(input$line_deselect_all)) {
      updateCheckboxGroupInput(session, "line_params", selected = character(0))
      updateCheckboxInput(session, "line_deselect_all", value = FALSE)
    }
  }, ignoreInit = TRUE)

  output$plot_line_ui <- renderUI({
    req(length(input$line_params) > 0)
    plotOutput(
      "plot_line",
      height = paste0(max(400, length(input$line_params) * 140), "px")
    )
  })
  
  # Combined text block — same as Preview block above, keyed by input$subject.
  output$plot_text <- renderPlot({
    req(input$subject)
    gg_text_tab <- text_plots_tab[[input$subject]][[1]] +
      theme(
        panel.border = element_blank(),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA)
      )
    gg_text_row <- text_plots[[input$subject]][[1]] +
      theme(
        panel.border = element_blank(),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_blank()
      )
    cowplot::plot_grid(
      gg_text_tab,
      gg_text_row,
      ncol = 1,
      rel_heights = c(0.5, 0.5)
    )
  })
  
  output$plot_line <- renderPlot({
    req(input$subject, length(input$line_params) > 0)

    line_plots <- subjectProfileLinePlot(
      data = adlbc_line,
      subjectSubset = input$subject,
      subsetVar = "PARAMN",
      subsetValue = as.integer(input$line_params),
      paramNameVar = "PARAM",
      paramValueVar = "AVAL",
      paramValueRangeVar = c("A1LO", "A1HI"),
      yLimFrom = "value",
      paramGroupVar = c("TESTN", "PT"),
      timeVar = "ADY_PLOT",
      title = "Laboratory test measurements",
      labelVars = labelVarsADaM,
      colorVar = "PT",
      colorPalette = c("AVAL" = "red", "CHG" = "blue"),
      shapeVar = "PT",
      shapePalette = c("AVAL" = 25, "CHG" = 23),
      paging = FALSE
    )

    pages <- line_plots[[input$subject]]
    # Do not use gg$data$ADY_PLOT — often missing; empty range → Inf/-Inf limits → geom drops rows.
    ady_rng <- adlbc_line %>%
      filter(
        .data$USUBJID == input$subject,
        .data$PARAMN %in% as.integer(input$line_params)
      ) %>%
      pull(.data$ADY_PLOT) %>%
      range(na.rm = TRUE)

    x_breaks <- pretty(ady_rng, n = 8)
    x_breaks <- x_breaks[x_breaks >= ady_rng[1] & x_breaks <= ady_rng[2]]

    pages <- lapply(pages, function(gg) {
      gg +
        scale_x_continuous(
          name = unname(labelVarsADaM["ADY"]),
          breaks = x_breaks,
          labels = function(x) ifelse(x == 0, "Baseline", as.character(x)),
          expand = expansion(mult = c(0.02, 0.02))
        ) +
        # coord_cartesian clips the panel; scale limits would drop rows and warn
        # (some subjects have pre-baseline ADY < 0 mapped to ADY_PLOT = 0).
        coord_cartesian(xlim = ady_rng) +
        theme(legend.position = "none")
    })

    if (length(pages) == 1) {
      print(pages[[1]])
    } else {
      cowplot::plot_grid(plotlist = pages, ncol = 1, align = "v")
    }
  })
  
  output$plot_interval <- renderPlot({
    req(input$subject)
    # Console warning from geom_point() here (not the line plot): some subjects
    # have AE rows with missing ASTDY or AENDY — see interval footnote and block above.
    pages <- interval_plots[[input$subject]]
    pages <- lapply(pages, function(gg) {
      gg +
        guides(
          color = guide_legend(
            nrow = 1, ncol = 6,
            override.aes = list(linetype = 1, shape = NA)
          ),
          shape = guide_legend(
            nrow = 1, ncol = 6,
            override.aes = list(linetype = NA)
          )
        ) +
        theme(
          legend.position = "bottom",
          legend.box = "vertical",
          legend.background = element_rect(fill = "white", colour = NA)
        )
    })
    if (length(pages) == 1) {
      print(pages[[1]])
    } else {
      cowplot::plot_grid(plotlist = pages, ncol = 1, align = "v")
    }
  })
}

shinyApp(ui = ui, server = server)
# Re-source the entire script (Ctrl+A → Source) before runApp if you stepped through blocks interactively.
