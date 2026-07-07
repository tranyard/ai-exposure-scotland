# =====================================================================
# 07_aps_employment.R  —  APS employment weights (individual microdata)
# =====================================================================
source(here::here("R", "00_config.R"))

V <- APS$vars

aps_micro_files <- function() {
  fs <- list.files(PATHS$aps, pattern = "^aps_micro_.*\\.csv$", full.names = TRUE)
  if (length(fs) == 0)
    stop("No APS microdata found. Put one person-level CSV per wave in ",
         PATHS$aps, " named aps_micro_<year>.csv (e.g. aps_micro_2022.csv).",
         call. = FALSE)
  fs
}

read_wave <- function(f) {
  d <- readr::read_csv(f, show_col_types = FALSE,
                       col_types = readr::cols(.default = readr::col_character()))
  need <- unlist(V[c("weight", "country", "ilo", "soc", "year")])
  miss <- setdiff(need, names(d))
  if (length(miss))
    stop("In ", basename(f), " missing expected columns: ",
         paste(miss, collapse = ", "),
         "\n  -> fix the names in APS$vars (00_config.R).", call. = FALSE)
  d
}
micro <- purrr::map_dfr(aps_micro_files(), read_wave)

# ---- Verification aid: weighted country composition
# Scotland should carry roughly 8% of UK person weight. If the share
# against APS$codes$scotland looks wrong, the COUNTRY coding assumption
# is wrong -- fix APS$codes$scotland before trusting any split.
ctab <- micro |>
  mutate(country = aps_num(.data[[V$country]]),
         w = aps_num(.data[[V$weight]])) |>
  filter(!is.na(country), !is.na(w), w > 0) |>
  count(country, wt = w, name = "w") |>
  mutate(share = w / sum(w))
message("weighted COUNTRY composition (verify Scotland = code ",
        APS$codes$scotland, ", expect ~8%):\n",
        paste(sprintf("  code %s: %.1f%%", ctab$country, 100 * ctab$share),
              collapse = "\n"))

# ---- Analysis base: employed persons ---------------------------------
base <- micro |>
  mutate(
    ilo     = aps_num(.data[[V$ilo]]),
    country = aps_num(.data[[V$country]]),
    soc_num = aps_num(.data[[V$soc]]),
    year    = as.integer(aps_num(.data[[V$year]])),
    w       = aps_num(.data[[V$weight]])) |>
  filter(ilo == APS$codes$in_employment) |>
  mutate(
    region = dplyr::if_else(country == APS$codes$scotland, "Scotland", "rUK"),
    soc_uk = substr(as.character(as.integer(soc_num)), 1, APS$soc_level)) |>
  filter(!is.na(region), !is.na(soc_uk), nzchar(soc_uk),
         !is.na(w), w > 0, year %in% APS$years)

# On a regional sub-slice Scotland can be absent; tolerate that only in
# the sandbox so the aggregation logic can still be exercised.
smoke <- nzchar(Sys.getenv("SMOKE_ROOT"))
if (!any(base$region == "Scotland")) {
  msg <- "No Scotland records after the employment/region filter."
  if (smoke) warning(msg, " (SMOKE_ROOT set: continuing on available regions.)")
  else stop(msg, " Check APS$codes$scotland against the weighted COUNTRY ",
            "tabulation above; the full all-nations APS is required.",
            call. = FALSE)
}

# ---- Year-level panel (region x soc x year)
aps_year <- base |>
  group_by(soc_uk, region, year) |>
  summarise(employment = sum(w), .groups = "drop")

# ---- Pooled across waves
aps_pool <- aps_year |>
  group_by(soc_uk, region) |>
  summarise(l_pool = sum(employment), .groups = "drop") |>
  group_by(region) |>
  mutate(sigma = l_pool / sum(l_pool)) |>    # within-region employment share
  ungroup()

stopifnot(all(abs(tapply(aps_pool$sigma, aps_pool$region, sum) - 1) < 1e-6))
message("APS pooled cells: ", nrow(aps_pool),
        " | regions: ", paste(sort(unique(aps_pool$region)), collapse = ", "),
        " | waves: ", paste(sort(unique(aps_year$year)), collapse = ", "))

save_csv(aps_pool, file.path(PATHS$cache, "aps_pool.csv"))
save_csv(aps_year, file.path(PATHS$cache, "aps_year.csv"))

# =====================================================================
# Worker-level wage extract for 15_wage_eiv.R
# =====================================================================
wage_need <- unlist(V[c("hourpay", "inc_weight", "age", "sex", "ft", "industry")])
wage_miss <- setdiff(wage_need, names(micro))
if (length(wage_miss)) {
  message("Wage extract skipped — missing: ", paste(wage_miss, collapse = ", "))
} else {
  wages <- micro |>
    mutate(
      ilo     = aps_num(.data[[V$ilo]]),
      country = aps_num(.data[[V$country]]),
      soc_num = aps_num(.data[[V$soc]]),
      hp      = aps_num(.data[[V$hourpay]]),
      piwt    = aps_num(.data[[V$inc_weight]]),
      age     = aps_num(.data[[V$age]]),
      sex     = aps_num(.data[[V$sex]]),
      ft      = aps_num(.data[[V$ft]]),
      industry = aps_num(.data[[V$industry]]),
      year    = as.integer(aps_num(.data[[V$year]]))) |>
    filter(ilo == APS$codes$in_employment,
           !is.na(hp), hp > 0, !is.na(piwt), piwt > 0,
           !is.na(soc_num), !is.na(sex), !is.na(ft), !is.na(industry)) |>
    mutate(region = dplyr::if_else(country == APS$codes$scotland,
                                   "Scotland", "rUK"),
           soc_uk = substr(as.character(as.integer(soc_num)), 1, APS$soc_level)) |>
    transmute(ln_wage = log(hp), soc_uk, region, age, sex, ft, industry,
              year, piwt)
  save_csv(wages, file.path(PATHS$cache, "aps_worker_wages.csv"))
  message("APS wage subsample: ", nrow(wages), " workers | ",
          "weighted mean hourly pay: ",
          sprintf("%.2f", weighted.mean(exp(wages$ln_wage), wages$piwt)))
}
