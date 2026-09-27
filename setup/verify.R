# Quick sanity check after setup (renv::restore() or setup/install_pkg.R).
# Run from the project opened via the .Rproj so the project library is active.

required_pkgs <- c(
  "patientProfilesVis",
  "clinUtils",
  "ggplot2",
  "dplyr",
  "shiny",
  "cowplot"
)

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package not available: ", pkg, call. = FALSE)
  }
  ver <- as.character(packageVersion(pkg))
  message(pkg, ": ", ver)
}

message("\nSetup OK — you can open scripts in exercises/.")

invisible(TRUE)
