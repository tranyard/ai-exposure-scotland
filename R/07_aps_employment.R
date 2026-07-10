# =====================================================================
# 07_aps_employment.R  —  APS employment weights (individual microdata)
#
# CHANGES:
#   1. COUNTRY fix: Scotland = codes {3, 4}. Code 4 (north of the
#      Caledonian Canal) was previously being assigned to rUK.
#   2. GOR9D retained -> comparator pools (Scotland / London /
#      rUK-ex-London, and per-GOR) for 16_comparators.R.
#   3. Unweighted respondent counts per cell -> aps_cell_counts.csv,
#      so precision claims are grounded, and thin cells can be flagged
#      in the FD employment regressions.
#   4. Person bootstrap of the within-region employment shares
#      (stratified by region x year, BOOT$B replicates) ->
#      aps_sigma_boot.csv, composed with scoring noise in 12.
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

# ---- Verification aid: weighted country composition -------------------
# Scotland (codes 3 + 4 combined) should carry roughly 8% of UK person
# weight, with code 4 a small fraction of that. If either looks wrong,
# fix APS$codes$scotland before trusting any split.
ctab <- micro |>
  mutate(country = aps_num(.data[[V$country]]),
         w = aps_num(.data[[V$weight]])) |>
  filter(!is.na(country), !is.na(w), w > 0) |>
  count(country, wt = w, name = "w") |>
  mutate(share = w / sum(w))
message("weighted COUNTRY composition (Scotland = codes ",
        paste(APS$codes$scotland, collapse = "+"), ", expect ~8% combined):\n",
        paste(sprintf("  code %s: %.1f%%", ctab$country, 100 * ctab$share),
              collapse = "\n"))
message(sprintf("  -> Scotland combined: %.1f%%",
        100 * sum(ctab$share[ctab$country %in% APS$codes$scotland])))

# ---- Analysis base: employed persons ---------------------------------
has_gor <- V$gor %in% names(micro)
if (!has_gor) message("NOTE: ", V$gor, " not on file — comparator pools skipped.")

base <- micro |>
  mutate(
    ilo     = aps_num(.data[[V$ilo]]),
    country = aps_num(.data[[V$country]]),
    soc_num = aps_num(.data[[V$soc]]),
    year    = as.integer(aps_num(.data[[V$year]])),
    w       = aps_num(.data[[V$weight]]),
    gor     = if (has_gor) as.character(.data[[V$gor]]) else NA_character_) |>
  filter(ilo == APS$codes$in_employment) |>
  mutate(
    region = region_of(country),                       ## CHANGED (3+4)
    soc_uk = substr(as.character(as.integer(soc_num)), 1, APS$soc_level)) |>
  filter(!is.na(region), !is.na(soc_uk), nzchar(soc_uk),
         !is.na(w), w > 0, year %in% APS$years)

smoke <- nzchar(Sys.getenv("SMOKE_ROOT"))
if (!any(base$region == "Scotland")) {
  msg <- "No Scotland records after the employment/region filter."
  if (smoke) warning(msg, " (SMOKE_ROOT set: continuing on available regions.)")
  else stop(msg, " Check APS$codes$scotland against the weighted COUNTRY ",
            "tabulation above; the full all-nations APS is required.",
            call. = FALSE)
}

# ---- Year-level panel (region x soc x year) ---------------------------
aps_year <- base |>
  group_by(soc_uk, region, year) |>
  summarise(employment = sum(w), .groups = "drop")

# ---- Pooled across waves ----------------------------------------------
aps_pool <- aps_year |>
  group_by(soc_uk, region) |>
  summarise(l_pool = sum(employment), .groups = "drop") |>
  group_by(region) |>
  mutate(sigma = l_pool / sum(l_pool)) |>
  ungroup()

stopifnot(all(abs(tapply(aps_pool$sigma, aps_pool$region, sum) - 1) < 1e-6))
message("APS pooled cells: ", nrow(aps_pool),
        " | regions: ", paste(sort(unique(aps_pool$region)), collapse = ", "),
        " | waves: ", paste(sort(unique(aps_year$year)), collapse = ", "))

save_csv(aps_pool, file.path(PATHS$cache, "aps_pool.csv"))
save_csv(aps_year, file.path(PATHS$cache, "aps_year.csv"))

# ---- NEW (3): unweighted respondent counts per cell --------------------
# n_unw is the pooled respondent count; n_min_year the thinnest single
# wave (matters for the first-difference employment regressions).
cell_counts <- base |>
  group_by(soc_uk, region, year) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(soc_uk, region) |>
  summarise(n_unw = sum(n), n_min_year = min(n), n_years = n(), .groups = "drop")
save_csv(cell_counts, file.path(PATHS$cache, "aps_cell_counts.csv"))
message("thin cells (pooled n_unw < 30): ",
        sum(cell_counts$n_unw < 30), " of ", nrow(cell_counts),
        " | Scotland cells with n_min_year < 10: ",
        sum(cell_counts$n_min_year < 10 & cell_counts$region == "Scotland"))

# ---- NEW (2): comparator pools (Scotland / London / rUK-ex-London) -----
if (has_gor) {
  base_cmp <- base |>
    mutate(comparator = case_when(
      region == "Scotland"            ~ "Scotland",
      gor == APS$codes$london_gor     ~ "London",
      TRUE                            ~ "rUK_exLondon"))
  # Verification aid: weighted comparator composition.
  wtab <- base_cmp |> count(comparator, wt = w, name = "w") |>
    mutate(share = w / sum(w))
  message("weighted comparator composition (London expect ~13-15% of employment):\n",
          paste(sprintf("  %s: %.1f%%", wtab$comparator, 100 * wtab$share),
                collapse = "\n"))

  cmp_pool <- base_cmp |>
    group_by(soc_uk, comparator) |>
    summarise(l_pool = sum(w), .groups = "drop") |>
    group_by(comparator) |>
    mutate(sigma = l_pool / sum(l_pool)) |>
    ungroup()
  save_csv(cmp_pool, file.path(PATHS$cache, "aps_comparator_pool.csv"))

  gor_pool <- base_cmp |>
    mutate(gor_lab = if_else(region == "Scotland", "Scotland", gor)) |>
    filter(!is.na(gor_lab), nzchar(gor_lab)) |>
    group_by(soc_uk, gor_lab) |>
    summarise(l_pool = sum(w), .groups = "drop") |>
    group_by(gor_lab) |>
    mutate(sigma = l_pool / sum(l_pool)) |>
    ungroup()
  save_csv(gor_pool, file.path(PATHS$cache, "aps_gor_pool.csv"))
}

# ---- NEW (4): person bootstrap of the within-region shares -------------
# Resamples persons with replacement within region x year strata and
# recomputes the pooled weighted shares. Captures APS sampling error in
# sigma — the component the Monte Carlo previously omitted entirely.
# Runtime: O(B x n); ~1-2 min at B = 500 on the pooled file.
message("bootstrapping APS shares (B = ", BOOT$B, ") ...")
set.seed(SEED + 7L)
cells   <- sort(unique(base$soc_uk))
boot_df <- map_dfr(sort(unique(base$region)), function(r) {
  br     <- base |> filter(region == r)
  strata <- split(seq_len(nrow(br)), br$year)
  cell_i <- match(br$soc_uk, cells)
  reps <- map_dfr(seq_len(BOOT$B), function(b) {
    idx <- unlist(lapply(strata, \(i) i[sample.int(length(i), length(i), replace = TRUE)]),
                  use.names = FALSE)
    tot <- rowsum(br$w[idx], cells[cell_i[idx]])       # weighted cell totals
    tibble(rep = b, soc_uk = rownames(tot),
           sigma_b = as.numeric(tot) / sum(tot))
  })
  reps$region <- r
  reps
})
save_csv(boot_df, file.path(PATHS$cache, "aps_sigma_boot.csv"))

# Quick summary of the sampling se this implies for each region's shares.
boot_se <- boot_df |>
  group_by(region, soc_uk) |>
  summarise(se_sigma = sd(sigma_b), .groups = "drop") |>
  group_by(region) |>
  summarise(median_se = median(se_sigma), max_se = max(se_sigma), .groups = "drop")
print(boot_se)

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
    mutate(region = region_of(country),                 ## CHANGED (3+4)
           soc_uk = substr(as.character(as.integer(soc_num)), 1, APS$soc_level)) |>
    transmute(ln_wage = log(hp), soc_uk, region, age, sex, ft, industry,
              year, piwt)
  save_csv(wages, file.path(PATHS$cache, "aps_worker_wages.csv"))
  message("APS wage subsample: ", nrow(wages), " workers | ",
          "weighted mean hourly pay: ",
          sprintf("%.2f", weighted.mean(exp(wages$ln_wage), wages$piwt)))
}
