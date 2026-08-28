# Fallback installer for workshop participants when renv::restore() is not usable.
# Installs into the active user library (not renv's project library).

repos <- "https://cloud.r-project.org"

required_pkgs <- c(
  "patientProfilesVis",
  "clinUtils",
  "tidyverse",
  "ggplot2",
  "dplyr"
)

missing <- required_pkgs[!vapply(required_pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]

if (length(missing) == 0L) {
  message("All required packages are already installed.")
} else {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = repos, dependencies = TRUE)
}

invisible(required_pkgs)
