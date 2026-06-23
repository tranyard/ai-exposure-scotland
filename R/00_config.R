# =====================================================================
# 00_config.R  —  Central configuration sourced by every script.
# Scotland vs rUK AI-exposure decomposition
# Edit paths and parameters here; nothing else hard-codes them.
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)   # dplyr, tidyr, readr, purrr, stringr, ggplot2
  library(here)        # project-root-relative paths
  library(digest)      # SHA-256 prompt hashing
})

# ---- Paths -----------------------------------------------------------
ROOT <- if (nzchar(Sys.getenv("SMOKE_ROOT"))) Sys.getenv("SMOKE_ROOT") else here()
PATHS <- list(
  onet        = file.path(ROOT, "data", "onet"),
  crosswalks  = file.path(ROOT, "data", "crosswalks"),
  aps         = file.path(ROOT, "data", "aps"),
  sfc         = file.path(ROOT, "data", "sfc"),
  scores      = file.path(ROOT, "out", "scores"),
  tables      = file.path(ROOT, "tables"),
  figures     = file.path(ROOT, "figures"),
  cache       = file.path(ROOT, "out", "cache")
)
invisible(lapply(PATHS, dir.create, recursive = TRUE, showWarnings = FALSE))

# ---Models (pin exact snapshots before the production run)
# M  = baseline model used for the full task set and the within-model variants
# M' = alternative model for the cross-model bias test
MODELS <- list(
  M  = list(provider = "anthropic", id = "claude-opus-4-8"),
  Mp = list(provider = "openai",    id = "gpt-4o-2024-11-20")
)

# ---- Scoring settings (frozen) --------------------------------------
SCORING <- list(
  temperature = 0,
  max_tokens  = 150,
  horizon_years_baseline = 10,     # V0 horizon
  horizon_years_short    = 5,      # V1 horizon
  anthropic_version = "2023-06-01", # current API version
  concurrency = 16,                # for synchronous (calibration) calls
  retries = 2
)

# ---- Classification thresholds (OBR baseline + sensitivity) ----------
THRESHOLDS <- list(
  central = c(sub = 70, comp = 40), #OBR
  low     = c(sub = 60, comp = 30),
  high    = c(sub = 80, comp = 50)
)

# ---- Calibration / propagation parameters ---------------------------
CALIB <- list(
  n_occupations = 90,
  per_major_group = 9,       # 10 major groups x 9
  variants = c("V0", "V1", "V2", "V3", "V4"),
  within_model_variants = c("V0", "V1", "V2", "V3")   # sigma^2_g uses these only
)
MONTECARLO <- list(
  R = 1000L,                 # number of draws
  rho_grid = c(0, 0.2, 0.4), # intra-group error correlation robustness
  boundary_lo = 15, boundary_hi = 85
)

# ---- APS pooling -----------------------------------------------------
APS <- list(
  years = c(2022, 2023, 2024, 2025),
  soc_level = 3L,
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
# Reads a single DIRECT mapping (e.g. mapping SOC2020-ONET2019)
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
