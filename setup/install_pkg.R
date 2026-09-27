# Fallback installer when renv::restore() is not usable.
# Run this from the project opened via the .Rproj file so renv is active:
# packages then install into the project library (renv/library/), not the user library.

repos <- "https://cloud.r-project.org"

# shiny: not an Imports of patientProfilesVis, but listed in its Suggests
# (optional Shiny report messaging). Workshop Shiny demos need it explicitly.
required_pkgs <- c(
  "patientProfilesVis",
  "clinUtils",
  "tidyverse",
  "ggplot2",
  "dplyr",
  "shiny",
  "cowplot"
)

missing <- required_pkgs[!vapply(required_pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]

if (length(missing) == 0L) {
  message("All required packages are already installed.")
} else {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = repos, dependencies = TRUE)
}

invisible(required_pkgs)
