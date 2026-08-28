# rPharma 2026 — patientProfilesVis Workshop

Hands-on exercises for building patient profile plots with [`patientProfilesVis`](https://github.com/openanalytics/patientProfilesVis) and ADaM example data from [`clinUtils`](https://cran.r-project.org/package=clinUtils).

## Repository layout

| Path | Description |
|------|-------------|
| `exercises/` | Workshop scripts (start here) |
| `renv.lock` | Pinned package versions (built with **R 4.5.1**) |
| `renv/` | renv project infrastructure (`activate.R`, etc.; **not** `renv/library/`) |
| `setup/install_pkg.R` | Fallback installer when `renv::restore()` is not usable |
| `setup/verify.R` | Quick check that required packages load |

## Setup

### Requirements

- **R ≥ 4.4** recommended (lockfile built with **R 4.5.1** on Windows)
- **RStudio** (optional but recommended)
- Internet access to download packages from CRAN

> **R version note:** You do *not* need the exact same patch release (e.g. 4.5.1 vs 4.5.2 is usually fine). Stay on the **same major version** when possible (R **4.5.x**). If you use a different major version (e.g. R 4.4 vs 4.5), `renv::restore()` may still work but can fail when pre-built binaries are unavailable for your R version.

### Get the materials

1. Download this repository as a ZIP from GitHub (**Code → Download ZIP**), or clone it.
2. Unzip if needed and open **`rpharma-2026-ppvis-workshop.Rproj`** in RStudio.

Opening the `.Rproj` file sets the working directory to the project root and runs **`.Rprofile`**, which activates renv. If you see a startup message about renv, that is expected.

### Option 1 — renv (recommended)

In the R console (project root as working directory):

```r
# Install renv once if you do not have it yet
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

renv::restore()
```

Then verify:

```r
source("setup/verify.R")
```

### Option 2 — manual install (fallback)

Use this if **`renv::restore()` fails** (common when your R version differs from the lockfile, or on restricted networks), or if you prefer not to use renv.

```r
source("setup/install_pkg.R")
source("setup/verify.R")
```

Packages are installed into your **user library** (not an isolated renv library). Workshop scripts should still run.

### Troubleshooting

| Symptom | What to do |
|---------|------------|
| Error on project open: `cannot open file 'renv/activate.R'` | Ensure the full repo was downloaded, including the `renv/` folder (not only `renv.lock`). Re-download the ZIP or pull latest from GitHub. |
| Error: `there is no package called 'renv'` | Run `install.packages("renv")`, then `renv::restore()`. |
| `renv::restore()` fails with R version / binary warnings | Switch to **Option 2**: `source("setup/install_pkg.R")`. |
| `library(patientProfilesVis)` fails after restore | Run `source("setup/install_pkg.R")`, then `source("setup/verify.R")`. |
| Red error text when opening `.Rproj` | Usually safe to ignore **after** you fix the environment in the console using Option 1 or 2. You do **not** need to avoid RStudio — run the setup commands above in the same session. |

For workshop support, capture:

```r
sessionInfo()
renv::status()   # if using renv
```

## Exercises

Open scripts under `exercises/` in order, or as directed in the workshop:

| Script | Topic |
|--------|--------|
| `pp_demo_adam.R` | Overview / getting started |
| `pp_demo_adam_line.R` | Line profile plots |
| `pp_demo_adam_interval.R` | Interval plots |
| `pp_demo_adam_text.R` | Text annotations |
| `ggplot_recipe.R` | ggplot2 customization patterns |

## Reference versions (from `renv.lock`)

| Package | Version |
|---------|---------|
| patientProfilesVis | 2.0.10 |
| clinUtils | 0.2.2 |
| tidyverse | (see lockfile) |

## License

MIT — see [LICENSE](LICENSE).
