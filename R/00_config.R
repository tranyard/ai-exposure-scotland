# =====================================================================
# 00_config.R  —  Central configuration sourced by every script.
# Scotland vs rUK AI-exposure decomposition (revised plan).
# Edit paths and parameters here; nothing else hard-codes them.
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)   # dplyr, tidyr, readr, purrr, stringr, ggplot2
  library(here)        # project-root-relative paths
  library(digest)      # SHA-256 prompt hashing
})

# ---- Paths -----------------------------------------------------------
# Root is the project unless SMOKE_ROOT is set (used by the smoke test to
# redirect all data/output into an isolated sandbox). Code is always read
# from the real repo via here(); only data/outputs are redirected.
ROOT <- if (nzchar(Sys.getenv("SMOKE_ROOT"))) Sys.getenv("SMOKE_ROOT") else here()
PATHS <- list(
  onet        = file.path(ROOT, "data", "onet"),
  crosswalks  = file.path(ROOT, "data", "crosswalks"),
  aps         = file.path(ROOT, "data", "aps"),
  sfc         = file.path(ROOT, "data", "sfc"),
  scores      = file.path(ROOT, "out", "scores"),
  tables      = file.path(ROOT, "tables"),           # repo root — LaTeX reads ../tables
  figures     = file.path(ROOT, "figures"),           # repo root — LaTeX reads ../figures
  cache       = file.path(ROOT, "out", "cache")
)
invisible(lapply(PATHS, dir.create, recursive = TRUE, showWarnings = FALSE))

# ---- Models (pin exact snapshots before the production run) ----------
# M  = baseline model used for the full task set and the within-model variants.
# M' = alternative model for the cross-model bias test (Plan Cor. 1).
# For a meaningful cross-model test, M' should be a genuinely different model
# (ideally a different provider). Pin the dated snapshot, not the alias.
MODELS <- list(
  M  = list(provider = "anthropic", id = "claude-opus-4-8"),          # <- pin snapshot
  Mp = list(provider = "openai",    id = "gpt-4o-2024-11-20")          # <- cross-model M'
)

# ---- Scoring settings (frozen) --------------------------------------
SCORING <- list(
  temperature = 0,
  max_tokens  = 150,
  horizon_years_baseline = 10,     # V0 horizon
  horizon_years_short    = 5,      # V1 horizon
  anthropic_version = "2023-06-01",
  concurrency = 16,                # for synchronous (calibration) calls
  retries = 2
)

# ---- Classification thresholds (OBR baseline + sensitivity) ----------
THRESHOLDS <- list(
  central = c(sub = 70, comp = 40),
  low     = c(sub = 60, comp = 30),
  high    = c(sub = 80, comp = 50)
)

# ---- Calibration / propagation parameters ---------------------------
CALIB <- list(
  n_occupations = 90,        # stratified sample size (Plan §2.4)
  per_major_group = 9,       # 10 major groups x 9
  variants = c("V0", "V1", "V2", "V3", "V4"),
  within_model_variants = c("V0", "V1", "V2", "V3")   # sigma^2_g uses these only
)
MONTECARLO <- list(
  R = 1000L,                 # number of draws (Plan §2.6)
  rho_grid = c(0, 0.2, 0.4), # intra-group error correlation robustness
  boundary_lo = 15, boundary_hi = 85
)

# ---- APS pooling -----------------------------------------------------
APS <- list(
  years = c(2022, 2023, 2024),   # SOC 2020 coded; avoids the SOC10 crosswalk step
  soc_level = 3L,                # 3-digit minor groups for the primary analysis
  regions = c("Scotland", "rUK")
)

# ---- Reproducibility -------------------------------------------------
SEED <- 20260615L
set.seed(SEED)

# ---- Small helpers ---------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

major_group <- function(soc_code) {
  # 1-digit SOC major group g(j): first digit of the (UK or US) SOC code.
  substr(gsub("[^0-9]", "", as.character(soc_code)), 1, 1)
}

save_csv <- function(x, file) {
  readr::write_csv(x, file)
  message("written: ", file, "  (", nrow(x), " rows)")
  invisible(x)
}

require_file <- function(path, hint = "") {
  if (!file.exists(path))
    stop("Missing input: ", path,
         if (nzchar(hint)) paste0("\n  -> ", hint) else "", call. = FALSE)
  path
}

# ---- Crosswalk loader: UK SOC 2020 unit group <-> O*NET-SOC code -----
# Reads a single DIRECT mapping (e.g. your "mapping SOC2020-ONET2019"
# sheet exported to CSV). Columns are identified by header text, so order
# does not matter. O*NET 30.3 uses the O*NET-SOC 2019 taxonomy, so a
# 2019-based concordance matches the task data.
read_uk_onet_map <- function() {
  f <- require_file(file.path(PATHS$crosswalks, "soc2020_to_onet.csv"),
       "Export the SOC2020<->O*NET-SOC sheet here. Needs a UK SOC2020 unit-group column and an O*NET-SOC code column.")
  raw <- readr::read_csv(f, show_col_types = FALSE)
  nm  <- tolower(names(raw))
  ci_uk <- which(stringr::str_detect(nm, "unit group|soc.?2020"))[1]
  ci_on <- which(stringr::str_detect(nm, "net.*code|o.?net.?soc"))[1]
  if (is.na(ci_uk) || is.na(ci_on))
    stop("Could not locate unit-group / O*NET-code columns in ", f,
         "\n  found columns: ", paste(names(raw), collapse = ", "), call. = FALSE)
  out <- tibble(soc_uk4 = stringr::str_trim(as.character(raw[[ci_uk]])),
                onet_soc_code = stringr::str_trim(as.character(raw[[ci_on]])))
  if (any(stringr::str_detect(nm, "weight")))
    out$weight <- as.numeric(raw[[which(stringr::str_detect(nm, "weight"))[1]]])
  out |> filter(!is.na(soc_uk4), soc_uk4 != "", !is.na(onet_soc_code), onet_soc_code != "") |>
    distinct()
}

message("config loaded | baseline model = ", MODELS$M$id,
        " | cross-model = ", MODELS$Mp$id)
