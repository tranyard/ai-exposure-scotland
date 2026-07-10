# =====================================================================
# 08_region_indices_shiftshare.R
#
# CHANGES:
#   1. The CONTINUOUS gap is now the headline object (printed first,
#      written to region_indices_continuous.csv); the classified shares
#      remain but are explicitly the threshold-sensitivity exhibit.
#   2. NEW channel-mix table (channel_mix.csv): classified substituted/
#      complemented share gaps alongside the continuous substitution-
#      and complementarity-operator gaps — the S/C composition of the
#      differential is a named result, not a by-product.
#   3. NEW primary-mode robustness row (pm_robustness.csv): the exposed-
#      share gap under the model's own primary_mode classification,
#      threshold-free.
#   4. NEW decomposition table (gap_decomposition.csv): per-SOC3
#      contributions dsigma_k (E_k - Ebar_rUK), ranked — the anatomy of
#      the gap for the results section and the hero figure.
# =====================================================================
source(here::here("R", "00_config.R"))

aps <- read_csv(file.path(PATHS$cache, "aps_pool.csv"), show_col_types = FALSE) |>
  mutate(soc_uk = as.character(soc_uk))

build_panel <- function(set) {
  suffix <- if (set == "central") "" else paste0("_", set)
  uk <- read_csv(file.path(PATHS$cache, paste0("uk_soc3_scores", suffix, ".csv")),
                 show_col_types = FALSE) |>
    mutate(soc_uk = as.character(soc_uk))
  aps |>
    inner_join(uk, by = "soc_uk") |>
    mutate(exposed = classification %in% c("Substituted", "Complemented"),
           sub  = classification == "Substituted",
           comp = classification == "Complemented",
           exposed_pm = classification_pm %in% c("Substituted", "Complemented"),
           sub_pm  = classification_pm == "Substituted",
           comp_pm = classification_pm == "Complemented")
}

region_indices <- function(panel) {
  panel |> group_by(region) |> summarise(
    E_hat = sum(sigma * exposed),
    S_hat = sum(sigma * sub),
    C_hat = sum(sigma * comp),
    E_bar = sum(sigma * E_uk / 100),
    .groups = "drop")
}

panel <- build_panel("central")
ri <- region_indices(panel)

# --- 1. HEADLINE: continuous indices and gap ---------------------------
ops <- c(max = "E_uk", sub = "E_uk_sub", comp = "E_uk_comp", sat = "E_uk_sat")
op_gap <- imap_dfr(ops, \(col, nm)
  panel |> group_by(region) |>
    summarise(idx = sum(sigma * .data[[col]] / 100), .groups = "drop") |>
    pivot_wider(names_from = region, values_from = idx) |>
    transmute(operator = nm, Scotland, rUK, gap = Scotland - rUK,
              gap_rel = gap / rUK))
save_csv(op_gap, file.path(PATHS$tables, "region_indices_continuous.csv"))
save_csv(op_gap, file.path(PATHS$tables, "operator_robustness.csv"))  # kept for back-compat
message("HEADLINE continuous gap (max operator): ",
        sprintf("%.4f (%.1f%% relative) | sub %.4f | comp %.4f | sat %.4f",
                op_gap$gap[op_gap$operator == "max"],
                100 * op_gap$gap_rel[op_gap$operator == "max"],
                op_gap$gap[op_gap$operator == "sub"],
                op_gap$gap[op_gap$operator == "comp"],
                op_gap$gap[op_gap$operator == "sat"]))

# --- Classified shares (threshold-sensitivity exhibit) -----------------
save_csv(ri, file.path(PATHS$tables, "region_indices.csv"))
gap <- ri |> summarise(
  dE_hat = E_hat[region == "Scotland"] - E_hat[region == "rUK"],
  dE_bar = E_bar[region == "Scotland"] - E_bar[region == "rUK"])
message(sprintf("classified dE_hat = %.4f (threshold-sensitive; see threshold_sensitivity.csv) | dE_bar = %.4f",
                gap$dE_hat, gap$dE_bar))

# --- 2. NEW: channel-mix table -----------------------------------------
wideri <- ri |> pivot_wider(names_from = region,
                            values_from = c(E_hat, S_hat, C_hat, E_bar))
channel_mix <- tibble(
  measure = c("classified substituted share", "classified complemented share",
              "continuous substitution index", "continuous complementarity index"),
  Scotland = c(wideri$S_hat_Scotland, wideri$C_hat_Scotland,
               op_gap$Scotland[op_gap$operator == "sub"],
               op_gap$Scotland[op_gap$operator == "comp"]),
  rUK      = c(wideri$S_hat_rUK, wideri$C_hat_rUK,
               op_gap$rUK[op_gap$operator == "sub"],
               op_gap$rUK[op_gap$operator == "comp"])) |>
  mutate(gap = Scotland - rUK, gap_rel = gap / rUK)
save_csv(channel_mix, file.path(PATHS$tables, "channel_mix.csv"))
message("channel mix: substituted-share gap ",
        sprintf("%+.4f (%.1f%% rel) vs continuous-substitution gap %+.4f",
                channel_mix$gap[1], 100 * channel_mix$gap_rel[1], channel_mix$gap[3]))

# --- 3. NEW: primary-mode robustness -----------------------------------
pm <- panel |> group_by(region) |> summarise(
  E_hat_pm = sum(sigma * exposed_pm),
  S_hat_pm = sum(sigma * sub_pm),
  C_hat_pm = sum(sigma * comp_pm), .groups = "drop")
pm_wide <- pm |> pivot_wider(names_from = region,
                             values_from = c(E_hat_pm, S_hat_pm, C_hat_pm))
pm_gap <- pm_wide |>
  transmute(dE_hat_pm = E_hat_pm_Scotland - E_hat_pm_rUK,
            dS_hat_pm = S_hat_pm_Scotland - S_hat_pm_rUK,
            dC_hat_pm = C_hat_pm_Scotland - C_hat_pm_rUK)
save_csv(bind_cols(pm_wide, pm_gap), file.path(PATHS$tables, "pm_robustness.csv"))
message(sprintf("primary-mode classification gaps: dE %+0.4f | dS %+0.4f | dC %+0.4f",
                pm_gap$dE_hat_pm, pm_gap$dS_hat_pm, pm_gap$dC_hat_pm))

# --- 4. NEW: per-occupation decomposition of the continuous gap --------
# dE_bar = sum_k dsigma_k (E_k - Ebar_rUK); demeaning is innocuous
# (sum dsigma = 0) and makes contributions interpretable.
wide_s <- panel |> select(soc_uk, region, sigma, E_uk, classification) |>
  pivot_wider(names_from = region, values_from = sigma,
              values_fill = list(sigma = 0), names_prefix = "sigma_")
Ebar_rUK <- with(wide_s, sum(sigma_rUK * E_uk / 100))
decomp <- wide_s |>
  mutate(dsigma = sigma_Scotland - sigma_rUK,
         contribution = dsigma * (E_uk / 100 - Ebar_rUK)) |>
  arrange(contribution) |>
  select(soc_uk, classification, E_uk, sigma_Scotland, sigma_rUK, dsigma, contribution)
save_csv(decomp, file.path(PATHS$tables, "gap_decomposition.csv"))
message("gap decomposition: check sum = ",
        sprintf("%.5f (headline %.5f)", sum(decomp$contribution),
                op_gap$gap[op_gap$operator == "max"]))

# --- Shift-share (unchanged) -------------------------------------------
wide <- panel |>
  select(soc_uk, region, sigma, exposed) |>
  pivot_wider(names_from = region, values_from = c(sigma, exposed),
              values_fill = list(sigma = 0)) |>
  mutate(exposed_rUK = coalesce(exposed_rUK, exposed_Scotland),
         exposed_Scotland = coalesce(exposed_Scotland, exposed_rUK))
shiftshare <- wide |> summarise(
  composition = sum((sigma_Scotland - sigma_rUK) * exposed_rUK),
  within      = sum(sigma_rUK * (exposed_Scotland - exposed_rUK)),
  total       = composition + within)
save_csv(shiftshare, file.path(PATHS$tables, "shiftshare.csv"))

# --- Threshold sensitivity of the classified gap (unchanged) -----------
thr_sens <- map_dfr(names(THRESHOLDS), \(set)
  region_indices(build_panel(set)) |>
    select(region, E_hat) |>
    pivot_wider(names_from = region, values_from = E_hat) |>
    transmute(thr_set = set, dE_hat = Scotland - rUK))
save_csv(thr_sens, file.path(PATHS$tables, "threshold_sensitivity.csv"))
message("classified gap across thresholds (fragility exhibit): ",
        paste(thr_sens$thr_set, sprintf("%+.4f", thr_sens$dE_hat),
              sep = "=", collapse = " "))

# --- Sector exposure rates (unchanged) ---------------------------------
sector <- panel |>
  mutate(major = major_group(soc_uk)) |>
  group_by(region, major) |>
  summarise(E_rs = sum(l_pool * exposed) / sum(l_pool), emp = sum(l_pool), .groups = "drop")
save_csv(sector, file.path(PATHS$tables, "sector_exposure.csv"))

save_csv(panel, file.path(PATHS$cache, "region_panel.csv"))
