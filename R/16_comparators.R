# =====================================================================
# 16_comparators.R  —  NEW: regional comparators (zero API cost)
#
# The headline compares Scotland with rUK, but rUK includes London,
# whose concentration of high-exposure professional and financial
# occupations plausibly drives much of the negative gap. This script:
#   1. Recomputes the continuous index for Scotland / London /
#      rUK-ex-London and reports all pairwise gaps.
#   2. Ranks every GOR (plus Scotland) on the continuous index — the
#      "where does Scotland sit in the UK league table" exhibit.
#   3. Decomposes the Scotland vs rUK-ex-London gap by SOC3.
# Requires the updated 07 (aps_comparator_pool.csv, aps_gor_pool.csv).
# =====================================================================
source(here::here("R", "00_config.R"))

cmp_f <- file.path(PATHS$cache, "aps_comparator_pool.csv")
gor_f <- file.path(PATHS$cache, "aps_gor_pool.csv")
if (!file.exists(cmp_f))
  stop("aps_comparator_pool.csv missing — run the updated 07_aps_employment.R ",
       "(and check GOR9D is on the APS file).", call. = FALSE)

uk <- read_csv(file.path(PATHS$cache, "uk_soc3_scores.csv"), show_col_types = FALSE) |>
  mutate(soc_uk = as.character(soc_uk))

# ---- 1. Scotland / London / rUK-ex-London ------------------------------
cmp <- read_csv(cmp_f, show_col_types = FALSE) |>
  mutate(soc_uk = as.character(soc_uk)) |>
  inner_join(uk, by = "soc_uk")

idx <- cmp |> group_by(comparator) |>
  summarise(E_bar     = sum(sigma * E_uk / 100),
            E_bar_sub = sum(sigma * E_uk_sub / 100),
            E_hat     = sum(sigma * (classification %in% c("Substituted", "Complemented"))),
            S_hat     = sum(sigma * (classification == "Substituted")),
            emp       = sum(l_pool),
            .groups = "drop")
save_csv(idx, file.path(PATHS$tables, "comparator_indices.csv"))

pairgap <- function(a, b, col)
  idx[[col]][idx$comparator == a] - idx[[col]][idx$comparator == b]
gaps <- tibble(
  contrast = c("Scotland - rUK_exLondon", "Scotland - London", "London - rUK_exLondon"),
  dE_bar = c(pairgap("Scotland", "rUK_exLondon", "E_bar"),
             pairgap("Scotland", "London", "E_bar"),
             pairgap("London", "rUK_exLondon", "E_bar")),
  dE_bar_sub = c(pairgap("Scotland", "rUK_exLondon", "E_bar_sub"),
                 pairgap("Scotland", "London", "E_bar_sub"),
                 pairgap("London", "rUK_exLondon", "E_bar_sub")),
  dS_hat = c(pairgap("Scotland", "rUK_exLondon", "S_hat"),
             pairgap("Scotland", "London", "S_hat"),
             pairgap("London", "rUK_exLondon", "S_hat")))
save_csv(gaps, file.path(PATHS$tables, "comparator_gaps.csv"))
message("comparator gaps (continuous, max operator):\n",
        paste(sprintf("  %s: %+.4f", gaps$contrast, gaps$dE_bar), collapse = "\n"))

# ---- 2. GOR league table ------------------------------------------------
if (file.exists(gor_f)) {
  gor <- read_csv(gor_f, show_col_types = FALSE) |>
    mutate(soc_uk = as.character(soc_uk)) |>
    inner_join(uk, by = "soc_uk") |>
    group_by(gor_lab) |>
    summarise(E_bar = sum(sigma * E_uk / 100), emp = sum(l_pool), .groups = "drop") |>
    arrange(desc(E_bar)) |>
    mutate(rank = row_number())
  save_csv(gor, file.path(PATHS$tables, "gor_ranking.csv"))
  message("GOR ranking (continuous index): Scotland is rank ",
          gor$rank[gor$gor_lab == "Scotland"], " of ", nrow(gor))
}

# ---- 3. Decomposition: Scotland vs rUK-ex-London ------------------------
wide <- cmp |>
  select(soc_uk, comparator, sigma, E_uk, classification) |>
  filter(comparator %in% c("Scotland", "rUK_exLondon")) |>
  pivot_wider(names_from = comparator, values_from = sigma,
              values_fill = list(sigma = 0))
Ebar_ref <- with(wide, sum(rUK_exLondon * E_uk / 100))
dec <- wide |>
  mutate(dsigma = Scotland - rUK_exLondon,
         contribution = dsigma * (E_uk / 100 - Ebar_ref)) |>
  arrange(contribution) |>
  select(soc_uk, classification, E_uk, sigma_Scotland = Scotland,
         sigma_rUK_exLondon = rUK_exLondon, dsigma, contribution)
save_csv(dec, file.path(PATHS$tables, "comparator_decomposition.csv"))
hl_f <- file.path(PATHS$tables, "region_indices_continuous.csv")
hl_gap <- if (file.exists(hl_f)) {
  read_csv(hl_f, show_col_types = FALSE) |>
    filter(operator == "max") |> pull(gap)
} else NA_real_
message(sprintf("Scotland vs rUK-ex-London gap: %+.4f (decomposition sum %+.4f) | vs full rUK: %+.4f",
                gaps$dE_bar[gaps$contrast == "Scotland - rUK_exLondon"],
                sum(dec$contribution), hl_gap))
