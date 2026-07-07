# =====================================================================
# 00_config.R  —  Central configuration sourced by every script.
# Edit paths and parameters here;
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
# M   = baseline annotator
# Mp  = first alternative model (different provider) for Corollary 1.
# Mpp = second alternative model (same provider, different tier), so the
#       cross-model stability claim rests on two contrasts rather than one.
MODELS <- list(
  M   = list(provider = "anthropic", id = "claude-sonnet-4-6"),
  Mp  = list(provider = "openai",    id = "gpt-4o-2024-11-20"),
  Mpp = list(provider = "anthropic", id = "claude-haiku-4-5")
)

# ---- Scoring settings (frozen) --------------------------------------
SCORING <- list(
  temperature = 0,
  max_tokens  = 150,
  base_year   = 2026,
  horizon_years_baseline = 10,     # V0 horizon
  horizon_years_short    = 5,      # V1 horizon
  anthropic_version = "2023-06-01",
  concurrency = 8,                 # sync workers; a fresh API account at a low
                                   # usage tier throttles hard above this
  retries = 2
)

# ---- Exposure operators
# max : headline composite -> OBR benchmar
# sub : substitution-only.
# sat : saturating combination E_sat = s + c - sc/100.
OPERATORS <- list(
  max = function(sub, comp) pmax(sub, comp),
  sub = function(sub, comp) sub,
  sat = function(sub, comp) sub + comp - sub * comp / 100
)

# ---- Classification stability
# flag: occupations with S_j below this are flagged "uncertain"
# grid: cutoff sweep for the invariance-of-the-gap table.
STAB <- list(
  flag = 0.75,
  grid = c(0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90)
)

# ---- Classification --------------------------------------------------
#   substituted   if sub >= thr["sub"]
#   complemented  if comp >= thr["comp"] and sub < thr["sub"]
#   unexposed     otherwise
# majority of the exposed mass.
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

# ---- Calibration / propagation parameters ---------------------------
# Variant scheme (Table 1 of the paper):
#   V0 baseline; V2 criteria reversed; V5 paraphrased rubric; V6 reordered
#   user message  -> meaning-preserving perturbations = the NOISE set.
#   V1 5-year horizon; V3 single composite score -> ESTIMAND sensitivity,
#   reported separately, never pooled into sigma^2_g.
CALIB <- list(
  n_occupations   = 90,
  n_random        = 10,       # sigma^2_g estimated on this draw
  n_near_boundary = 8,        # held out to validate the CSS out of sample
  score_variants  = c("V1", "V2", "V3", "V5", "V6"),  # V0 reused from full run
  noise_variants  = c("V0", "V2", "V5", "V6"),
  sensitivity_variants = c("V1", "V3")
)
MONTECARLO <- list(
  R = 1000L,                 # number of draws
  rho_grid = c(0, 0.2, 0.4), # intra-group error correlation robustness
  boundary_lo = 15, boundary_hi = 85
)

# ---- APS pooling
# -9 does not apply, -8 no answer
#   ILODEFR: 1 In employment, 2 ILO unemployed, 3 Inactive
#   SEX:     1 Male, 2 Female       FTPT: 1 Full time, 2 Part time
#   SC20MMN: SOC 2020 minor-group numeric codes (e.g. 412)
#   INDS07M: SIC 2007 sections coded alphabetically (7 = G, 16 = P, ...)
#   COUNTRY: 1 England, 3 + 4(North of Caladonian Canal WE OMIT) = Scotland
APS <- list(
  years         = c(2022, 2023, 2024, 2025),
  soc_level     = 3L,
  regions       = c("Scotland", "rUK"),
  codes = list(
    in_employment = 1,
    scotland      = 3
  ),
  vars = list(
    weight     = "PWTA22",    # person weight  -> employment counts
    inc_weight = "PIWTA22",   # income weight  -> wage subsample only
    country    = "COUNTRY",
    ilo        = "ILODEFR",
    soc        = "SC20MMN",   # SOC2020 minor group, numeric
    year       = "REFWKY",    # reference-week year
    hourpay    = "HOURPAY",   # derived gross hourly pay (wage module)
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

message("config loaded | baseline model = ", MODELS$M$id,
        " | alternatives = ",
        paste(compact(map(MODELS[c("Mp","Mpp")], "id")), collapse = ", "))
