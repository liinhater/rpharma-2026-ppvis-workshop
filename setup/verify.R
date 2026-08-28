# Quick sanity check after setup (renv or manual install).

required_pkgs <- c(
  "patientProfilesVis",
  "clinUtils",
  "ggplot2",
  "dplyr"
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
