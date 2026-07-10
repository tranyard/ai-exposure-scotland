# =====================================================================
# 00_config.R  —  Central configuration sourced by every script.
# Edit paths and parameters here;
#
# CHANGES (July 2026 revision):
#   * APS$codes$scotland is now c(3, 4): COUNTRY code 4 (Scotland north
#     of the Caledonian Canal) was previously falling through to rUK.
#   * APS$vars$gor + APS$codes$london_gor: GOR9D retained for the
#     comparator analysis (16_comparators.R).
#   * BOOT: APS person-bootstrap parameters (07 writes replicate shares,
#     12 composes them with scoring noise).
#   * uk3_continuous(): shared O*NET -> SOC3 aggregation that mirrors
#     06_crosswalk EXACTLY (weighted mean to SOC4, unweighted mean to
#     SOC3). Used by 12 and 14 so every stage aggregates identically —
#     previously 12/14 used a direct unweighted O*NET -> SOC3 mean,
#     which shifted the MC baseline off the headline gap.
#   * classify_pm(): classification from the model's own primary_mode
#     votes (free robustness row against the threshold classifier).
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
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

# ---- Models -----------------------------------------------------------
MODELS <- list(
  M   = list(provider = "anthropic", id = "claude-sonnet-4-6"),
  Mp  = list(provider = "openai",    id = "gpt-4o-2024-11-20"),
  Mpp = list(provider = "anthropic", id = "claude-haiku-4-5-20251001")
)

# ---- Scoring settings (frozen) --------------------------------------
SCORING <- list(
  temperature = 0,
  max_tokens  = 150,
  base_year   = 2026,
  horizon_years_baseline = 10,
  horizon_years_short    = 5,
  anthropic_version = "2023-06-01",
  concurrency = 8,
  retries = 2
)

# ---- Exposure operators ----------------------------------------------
OPERATORS <- list(
  max = function(sub, comp) pmax(sub, comp),
  sub = function(sub, comp) sub,
  sat = function(sub, comp) sub + comp - sub * comp / 100
)

# ---- Classification stability ----------------------------------------
STAB <- list(
  flag = 0.75,
  grid = c(0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90)
)

# ---- Classification --------------------------------------------------
THRESHOLDS <- list(
  central = c(sub = 70, comp = 40),  # OBR baseline
  low     = c(sub = 60, comp = 30),
  high    = c(sub = 80, comp = 50)
)
EXP_MATERIALITY <- 0.5

classify_occ <- function(Sub, Comp, materiality = EXP_MATERIALITY) {
  Exp <- Sub + Comp
  dplyr::case_when(
    Exp < materiality       ~ "Unexposed",
    Sub > 0.5 * Exp         ~ "Substituted",
    TRUE                    ~ "Complemented"
  )
}

## NEW: classification from the model's own primary_mode field.
## PM_sub / PM_comp are importance-weighted shares of tasks the model
## itself labelled substitution / complementarity. Threshold-free.
classify_pm <- function(PM_sub, PM_comp, materiality = EXP_MATERIALITY) {
  Exp <- PM_sub + PM_comp
  dplyr::case_when(
    Exp < materiality  ~ "Unexposed",
    PM_sub > PM_comp   ~ "Substituted",
    TRUE               ~ "Complemented"
  )
}

# ---- Calibration / propagation parameters ---------------------------
CALIB <- list(
  n_occupations   = 90,
  n_random        = 10,
  n_near_boundary = 8,
  score_variants  = c("V1", "V2", "V3", "V5", "V6"),
  noise_variants  = c("V0", "V2", "V5", "V6"),
  sensitivity_variants = c("V1", "V3")
)
MONTECARLO <- list(
  R = 1000L,
  rho_grid = c(0, 0.2, 0.4),
  boundary_lo = 15, boundary_hi = 85
)

## NEW: APS person-bootstrap (sampling uncertainty in the employment
## shares). 07 writes BOOT$B replicate share vectors per region;
## 12 composes them with the scoring-noise draws.
BOOT <- list(
  B = as.integer(Sys.getenv("APS_BOOT_B", "500"))
)

# ---- APS pooling ------------------------------------------------------
# -9 does not apply, -8 no answer
#   ILODEFR: 1 In employment, 2 ILO unemployed, 3 Inactive
#   SEX:     1 Male, 2 Female       FTPT: 1 Full time, 2 Part time
#   SC20MMN: SOC 2020 minor-group numeric codes (e.g. 412)
#   INDS07M: SIC 2007 sections coded alphabetically (7 = G, 16 = P, ...)
#   COUNTRY: 1 England, 2 Wales, 3 Scotland, 4 Scotland north of the
#            Caledonian Canal, 5 Northern Ireland.
#            SCOTLAND = codes 3 AND 4. (Code 4 was previously being
#            assigned to rUK — fixed.)
#   GOR9D:   ONS 9-character region code; London = E12000007.
APS <- list(
  years         = c(2022, 2023, 2024, 2025),
  soc_level     = 3L,
  regions       = c("Scotland", "rUK"),
  codes = list(
    in_employment = 1,
    scotland      = c(3, 4),          ## CHANGED (was 3)
    london_gor    = "E12000007"       ## NEW
  ),
  vars = list(
    weight     = "PWTA22",
    inc_weight = "PIWTA22",
    country    = "COUNTRY",
    gor        = "GOR9D",             ## NEW
    ilo        = "ILODEFR",
    soc        = "SC20MMN",
    year       = "REFWKY",
    hourpay    = "HOURPAY",
    age        = "AGE",
    sex        = "SEX",
    ft         = "FTPT",
    industry   = "INDS07M"
  )
)

# Negative APS sentinels (-1, -8, -9) -> NA; leaves valid values numeric.
aps_num <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  replace(v, !is.na(v) & v < 0, NA_real_)
}

## NEW: single definition of the Scotland/rUK split, used everywhere.
region_of <- function(country) {
  dplyr::if_else(country %in% APS$codes$scotland, "Scotland", "rUK")
}

# ---- Reproducibility -------------------------------------------------
SEED <- 20260615L
set.seed(SEED)

# ---- Small helpers ---------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

major_group <- function(soc_code) {
  substr(gsub("[^0-9]", "", as.character(soc_code)), 1, 1)
}

save_csv <- function(x, file) {
  readr::write_csv(x, file)
  message("written: ", file, "  (", nrow(x), " rows)")
  invisible(x)
}

read_scores <- function(file) {
  readr::read_csv(file, show_col_types = FALSE) |>
    dplyr::mutate(run_date = as.character(run_date))
}

require_file <- function(path, hint = "") {
  if (!file.exists(path))
    stop("Missing input: ", path,
         if (nzchar(hint)) paste0("\n  -> ", hint) else "", call. = FALSE)
  path
}

# ---- Crosswalk loader: UK SOC 2020 unit group <-> O*NET-SOC code -----
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

## NEW: shared O*NET -> SOC3 continuous aggregation, mirroring
## 06_crosswalk EXACTLY: crosswalk-weighted mean O*NET -> SOC4, then
## unweighted mean SOC4 -> SOC3. Any script that needs a continuous
## SOC3 score from an occupation table (12_montecarlo, 14_common_mode)
## must use this rather than rolling its own, so that the propagated
## and cross-model objects are the same estimand as the headline.
uk3_continuous <- function(occ, cols = "E_j", map = NULL) {
  if (is.null(map)) map <- read_uk_onet_map()
  if (!"weight" %in% names(map)) map$weight <- 1
  occ <- occ |> dplyr::mutate(onet_soc_code = as.character(onet_soc_code))
  map |>
    dplyr::inner_join(occ, by = "onet_soc_code") |>
    dplyr::group_by(soc_uk4) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(cols),
                                   \(x) stats::weighted.mean(x, weight)),
                     .groups = "drop") |>
    dplyr::mutate(soc_uk = substr(gsub("[^0-9]", "", soc_uk4), 1, APS$soc_level)) |>
    dplyr::group_by(soc_uk) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(cols), mean), .groups = "drop")
}

message("config loaded | baseline model = ", MODELS$M$id,
        " | alternatives = ",
        paste(compact(map(MODELS[c("Mp","Mpp")], "id")), collapse = ", "),
        " | Scotland = COUNTRY %in% {", paste(APS$codes$scotland, collapse = ","), "}")
