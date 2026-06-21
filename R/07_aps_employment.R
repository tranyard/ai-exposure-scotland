# =====================================================================
# 07_aps_employment.R  —  APS employment weights (Plan §1.2/§1.3).
# Pools 2022-2024 APS employment counts by SOC 2020 (3-digit) x region,
# splits the UK into Scotland and rUK, and forms within-region shares
# sigma_{k,r}. Keeps the year-level panel for the first-difference spec.
# =====================================================================
source(here::here("R", "00_config.R"))

# Expect a NOMIS export: columns soc_uk, region (nations), year, employment.
aps_raw <- read_csv(
  require_file(file.path(PATHS$aps, "aps_employment_soc_region.csv"),
               "NOMIS > APS > employment count by SOC2020 minor group x nation x year"),
  show_col_types = FALSE) |>
  mutate(soc_uk = substr(gsub("[^0-9]", "", soc_uk), 1, APS$soc_level),
         region = if_else(region == "Scotland", "Scotland", "rUK")) |>
  filter(year %in% APS$years, !is.na(soc_uk), soc_uk != "")

# Year-level panel (region x soc x year), rUK aggregated across E/W/NI.
aps_year <- aps_raw |>
  group_by(soc_uk, region, year) |>
  summarise(employment = sum(employment, na.rm = TRUE), .groups = "drop")

# Pooled across years: treat the three waves as one larger survey.
aps_pool <- aps_year |>
  group_by(soc_uk, region) |>
  summarise(l_pool = sum(employment), .groups = "drop") |>
  group_by(region) |>
  mutate(sigma = l_pool / sum(l_pool)) |>   # within-region employment share
  ungroup()

stopifnot(all(abs(tapply(aps_pool$sigma, aps_pool$region, sum) - 1) < 1e-6))
message("APS pooled cells: ", nrow(aps_pool),
        " | Scotland SOC groups: ", sum(aps_pool$region == "Scotland"))

save_csv(aps_pool, file.path(PATHS$cache, "aps_pool.csv"))
save_csv(aps_year, file.path(PATHS$cache, "aps_year.csv"))
