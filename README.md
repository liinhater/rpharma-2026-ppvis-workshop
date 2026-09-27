# R/Pharma 2026 APAC Workshop — Hands-On Patient Profile Visualization
*Refining the R package patientProfilesVis with ggplot2, Interacting in shiny*

Hands-on exercises for building patient profile plots with [`patientProfilesVis`](https://github.com/openanalytics/patientProfilesVis) and ADaM example data from [`clinUtils`](https://cran.r-project.org/package=clinUtils).

## Repository layout

| Path | Description |
|------|-------------|
| `exercises/` | Workshop scripts (start here) |
| `renv.lock` | Pinned package versions (built with **R 4.5.1**) |
| `renv/` | renv project infrastructure (`activate.R`, etc.; **`renv/library/` is created locally and is not in git**) |
| `setup/install_pkg.R` | Fallback installer when `renv::restore()` is not usable |
| `setup/verify.R` | Quick check that required packages load |

## Setup

### Requirements

- **R ≥ 4.4** recommended (lockfile built with **R 4.5.1** on Windows)
- **RStudio** recommended (open the project via the `.Rproj` file)
- Internet access to download packages from CRAN

> **R version note:** You do *not* need the exact same patch release (e.g. 4.5.1 vs 4.5.2 is usually fine). Stay on the **same major.minor line** when possible (R **4.5.x**). If you use a different R version (e.g. 4.4 vs 4.5), `renv::restore()` may fail when binaries for your R version are unavailable — use Option 2 below.

### Get the materials

1. Download this repository as a ZIP from GitHub (**Code → Download ZIP**), or clone it.
2. Unzip if needed and open **`rpharma-2026-ppvis-workshop.Rproj`** in RStudio.

Opening the `.Rproj` file:

- sets the working directory to the project root, and
- runs **`.Rprofile`** → `renv/activate.R`, which activates the **project library** (`renv/library/`).

If `renv` itself is missing, the activator typically bootstraps/installs it for this project. Console messages about renv on first open are expected.

**Always work inside this RStudio project** for the workshop. Then both setup options below install packages into **`renv/library/`** (isolated project library), not your global user library.

### Option 1 — renv restore (recommended)

In the R console (project already open via `.Rproj`):

```r
renv::restore()
```

Then verify:

```r
source("setup/verify.R")
```

### Option 2 — install listed packages (fallback)

Use this if **`renv::restore()` fails** (different R version, missing binaries, or restricted network). Still run it **with the `.Rproj` project open** so installs go into `renv/library/`.

```r
source("setup/install_pkg.R")
source("setup/verify.R")
```

Option 2 installs the packages named in `setup/install_pkg.R` (and their dependencies) from CRAN. Versions may differ slightly from `renv.lock`; that is fine for the workshop exercises.

### Troubleshooting

These issues are uncommon if you download the full ZIP (or clone) and open the `.Rproj`. Use them only if something fails.

| Symptom | What to do |
|---------|------------|
| `cannot open file 'renv/activate.R'` when the project starts | The `renv/` folder is missing or incomplete. Re-download the full ZIP / re-clone (not only `renv.lock`). |
| `renv::restore()` fails (R version / binary / network) | Stay in the open `.Rproj` session and use Option 2: `source("setup/install_pkg.R")`. |
| `there is no package called '...'` when running an exercise | Run `source("setup/verify.R")`. If a package is missing, run Option 1 or 2 again in the same project session. |
| Packages seem to install into the wrong library | Confirm you opened **`rpharma-2026-ppvis-workshop.Rproj`** (not a loose `.R` file). Check with `.libPaths()` — the project `renv/library/` path should appear first. |

For workshop support, capture:

```r
sessionInfo()
.libPaths()
renv::status()
```

## Exercises

Open scripts under `exercises/` in order, or as directed in the workshop:

| Script | Topic |
|--------|--------|
| `pp_demo_adam.R` | Overview / getting started |
| `pp_demo_adam_line.R` | Line profile plots |
| `pp_demo_adam_interval.R` | Interval plots |
| `pp_demo_adam_text.R` | Text annotations |
| `pp_demo_adam_shiny.R` | Shiny: interactive subject preview |
| `pp_demo_adam_shiny_enhanced.R` | Shiny: enhanced interactive preview |
| `ggplot_recipe.R` | Helper to inspect ggplot objects |

## Reference versions (from `renv.lock`)

Versions pinned when the lockfile was built (R **4.5.1**). Option 1 restores these; Option 2 may get nearby CRAN versions.

| Package | Version |
|---------|---------|
| patientProfilesVis | 2.0.10 |
| clinUtils | 0.2.2 |
| ggplot2 | 4.0.3 |
| dplyr | 1.2.1 |
| shiny | 1.14.0 |
| cowplot | 1.2.0 |

## License

MIT — see [LICENSE](LICENSE).
