# APS employment weights from the person-level microdata: pooled and
# per-year SOC3 x region shares, respondent counts, comparator pools,
# occupation x industry pool, a person bootstrap of the shares, and the
# worker-level wage extract used by 15 and 20.
source(here::here("R", "00_config.R"))

V <- APS$vars

# GOR9D is read as character, so the APS negative sentinels arrive as
# strings. The devolved nations are coded W/S/N99999999: these are valid
# whole-nation region codes (GOR9D only subdivides England), so they must
# survive as their own FE levels.
clean_gor <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "-1", "-8", "-9", "NA", "N/A", ".")] <- NA_character_
  x
}

aps_micro_files <- function() {
  fs <- list.files(PATHS$aps, pattern = "^aps_.*\\.csv$", full.names = TRUE)
  if (length(fs) == 0)
    stop("No APS microdata found. Put one person-level CSV per wave in ",
         PATHS$aps, " named aps_<year>.csv (e.g. aps_2022.csv).",
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
  # Optional columns are not caught above: bind_rows silently NA-fills a
  # wave that lacks one, which is the classic cause of a geography
  # variable being ~1/n_waves missing overall. Surface it at read time.
  opt <- unlist(V[c("gor", "industry", "hourpay", "inc_weight",
                    "age", "sex", "ft", "education", "public")])
  opt <- opt[!is.na(opt)]
  opt_miss <- opt[!opt %in% names(d)]
  if (length(opt_miss))
    message("  ", basename(f), ": optional column(s) absent, NA-filled on pooling: ",
            paste(opt_miss, collapse = ", "))
  if (!is.na(V$gor) && !V$gor %in% names(d)) {
    alt <- grep("GOR", names(d), value = TRUE, ignore.case = TRUE)
    if (length(alt))
      message("    ", basename(f), ": '", V$gor, "' absent but GOR-like column(s) present: ",
              paste(alt, collapse = ", "))
  }
  d
}
micro <- purrr::map_dfr(aps_micro_files(), read_wave)

# Scotland (codes 3 + 4) should carry roughly 8% of UK person weight.
ctab <- micro |>
  mutate(country = aps_num(.data[[V$country]]),
         w = aps_num(.data[[V$weight]])) |>
  filter(!is.na(country), !is.na(w), w > 0) |>
  count(country, wt = w, name = "w") |>
  mutate(share = w / sum(w))
message(sprintf("weighted Scotland share of UK person weight: %.1f%%",
                100 * sum(ctab$share[ctab$country %in% APS$codes$scotland])))

# Region arrives under different names across waves (GOR9D to 2024,
# GOR9DCENSUS2021 in 2025) with identical values; accept either and
# coalesce, or the renamed wave is NA-filled and lost downstream.
gor_names   <- unique(c(V$gor, "GOR9DCENSUS2021"))
gor_present <- intersect(gor_names, names(micro))
has_gor     <- length(gor_present) > 0
if (has_gor) {
  g <- rep(NA_character_, nrow(micro))
  for (cn in gor_present) g <- dplyr::coalesce(g, clean_gor(micro[[cn]]))
  micro$gor <- g
} else {
  micro$gor <- NA_character_
  message("no region column on file - comparator pools skipped.")
}
has_ind <- V$industry %in% names(micro)
if (!has_ind) message(V$industry, " not on file - industry decomposition pool skipped.")
has_edu <- !is.null(V$education) && V$education %in% names(micro)
has_pub <- !is.null(V$public)    && V$public    %in% names(micro)
if (!has_edu) message("education variable not on file - omitted from the wage extract.")
if (!has_pub) message("public-sector variable not on file - omitted from the wage extract.")

base <- micro |>
  mutate(
    ilo     = aps_num(.data[[V$ilo]]),
    country = aps_num(.data[[V$country]]),
    soc_num = aps_num(.data[[V$soc]]),
    year    = as.integer(aps_num(.data[[V$year]])),
    w       = aps_num(.data[[V$weight]]),
    industry = if (has_ind) aps_num(.data[[V$industry]]) else NA_real_) |>
  filter(ilo == APS$codes$in_employment) |>
  mutate(
    region = region_of(country),
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

aps_year <- base |>
  group_by(soc_uk, region, year) |>
  summarise(employment = sum(w), .groups = "drop")

aps_pool <- aps_year |>
  group_by(soc_uk, region) |>
  summarise(l_pool = sum(employment), .groups = "drop") |>
  group_by(region) |>
  mutate(sigma = l_pool / sum(l_pool)) |>
  ungroup()

stopifnot(all(abs(tapply(aps_pool$sigma, aps_pool$region, sum) - 1) < 1e-6))
message("APS pooled cells: ", nrow(aps_pool),
        " | waves: ", paste(sort(unique(aps_year$year)), collapse = ", "))

save_csv(aps_pool, file.path(PATHS$cache, "aps_pool.csv"))
save_csv(aps_year, file.path(PATHS$cache, "aps_year.csv"))

# n_unw is the pooled respondent count; n_min_year the thinnest single
# wave, which matters for the first-difference employment regressions.
cell_counts <- base |>
  group_by(soc_uk, region, year) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(soc_uk, region) |>
  summarise(n_unw = sum(n), n_min_year = min(n), n_years = n(), .groups = "drop")
save_csv(cell_counts, file.path(PATHS$cache, "aps_cell_counts.csv"))
message("thin cells (pooled n_unw < 30): ",
        sum(cell_counts$n_unw < 30), " of ", nrow(cell_counts))

if (has_gor) {
  # Structural missingness (one wave lacking or renaming the column) shows
  # as a near-100% wave beside near-0% waves; flat missingness is genuine
  # non-response. n_levels flags a coding mismatch against london_gor.
  gor_by_year <- base |>
    filter(region == "rUK") |>
    group_by(year) |>
    summarise(n = n(), miss_share = mean(is.na(gor)),
              n_levels = dplyr::n_distinct(gor[!is.na(gor)]), .groups = "drop")
  bad_waves <- gor_by_year$year[gor_by_year$miss_share > 0.5]
  if (length(bad_waves))
    warning("GOR is >50% missing in wave(s) ", paste(bad_waves, collapse = ", "),
            " but present elsewhere: the column is probably absent or renamed ",
            "in those files and NA-filled on pooling.", call. = FALSE)
  london_n <- sum(base$gor == APS$codes$london_gor, na.rm = TRUE)
  if (london_n == 0)
    warning("No rows match london_gor = '", APS$codes$london_gor,
            "'. Set APS$codes$london_gor to the code this file uses.",
            call. = FALSE)
  base_cmp <- base |>
    mutate(comparator = case_when(
      region == "Scotland"            ~ "Scotland",
      gor == APS$codes$london_gor     ~ "London",
      TRUE                            ~ "rUK_exLondon"))

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

# SOC3 x SIC-section x region shares, within region, for the industry-mix
# vs within-industry decomposition in 18.
if (has_ind) {
  occind_pool <- base |>
    filter(!is.na(industry)) |>
    group_by(soc_uk, industry, region) |>
    summarise(l_pool = sum(w), .groups = "drop") |>
    group_by(region) |>
    mutate(sigma = l_pool / sum(l_pool)) |>
    ungroup()
  save_csv(occind_pool, file.path(PATHS$cache, "aps_occind_pool.csv"))
  ind_miss <- base |>
    summarise(s = sum(w[is.na(industry)]) / sum(w)) |> pull(s)
  message(sprintf("occ x industry pool: %d cells | %.2f%% of employment weight lacks industry",
                  nrow(occind_pool), 100 * ind_miss))
}

# Person bootstrap: resample within region x year strata and recompute the
# pooled weighted shares, giving the APS sampling component of sigma that
# 12 composes with the scoring noise.
set.seed(SEED + 7L)
cells   <- sort(unique(base$soc_uk))
boot_df <- map_dfr(sort(unique(base$region)), function(r) {
  br     <- base |> filter(region == r)
  strata <- split(seq_len(nrow(br)), br$year)
  cell_i <- match(br$soc_uk, cells)
  reps <- map_dfr(seq_len(BOOT$B), function(b) {
    idx <- unlist(lapply(strata, \(i) i[sample.int(length(i), length(i), replace = TRUE)]),
                  use.names = FALSE)
    tot <- rowsum(br$w[idx], cells[cell_i[idx]])
    tibble(rep = b, soc_uk = rownames(tot),
           sigma_b = as.numeric(tot) / sum(tot))
  })
  reps$region <- r
  reps
})
save_csv(boot_df, file.path(PATHS$cache, "aps_sigma_boot.csv"))

wage_need <- unlist(V[c("hourpay", "inc_weight", "age", "sex", "ft", "industry")])
wage_miss <- setdiff(wage_need, names(micro))
if (length(wage_miss)) {
  message("Wage extract skipped - missing: ", paste(wage_miss, collapse = ", "))
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
      education = if (has_edu) as.character(.data[[V$education]]) else NA_character_,
      public    = if (has_pub) aps_num(.data[[V$public]])        else NA_real_,
      year    = as.integer(aps_num(.data[[V$year]]))) |>
    filter(ilo == APS$codes$in_employment,
           !is.na(hp), hp > 0, !is.na(piwt), piwt > 0,
           !is.na(soc_num), !is.na(sex), !is.na(ft), !is.na(industry)) |>
    mutate(region = region_of(country),
           soc_uk = substr(as.character(as.integer(soc_num)), 1, APS$soc_level)) |>
    transmute(ln_wage = log(hp), soc_uk, region, gor, age, sex, ft, industry,
              year, piwt, education, public)
  save_csv(wages, file.path(PATHS$cache, "aps_worker_wages.csv"))
  message("APS wage subsample: ", nrow(wages), " workers")
}
