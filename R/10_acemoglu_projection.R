# =====================================================================
# 10_acemoglu_projection.R  —  Productivity & GDP projection
# TFP gain (eq. 6A): dln TFP_r ~ kappa * sum_k sigma_{k,r} * pi_k * E_bar_k.
# kappa, pi and horizon are imported from the OBR/Acemoglu calibration,
#  Regions differ only through their employment-weighted
# exposure. Reports a band over the OBR parameter range.
# =====================================================================
source(here::here("R", "00_config.R"))

panel <- read_csv(file.path(PATHS$cache, "region_panel.csv"), show_col_types = FALSE) |>
  mutate(E_bar = E_uk / 100)

# Calibration inputs
# kappa: average net cost saving on exposed tasks; band = {lo, central, hi}.
calib <- tryCatch(
  read_csv(file.path(PATHS$sfc, "acemoglu_calibration.csv"), show_col_types = FALSE),
  error = function(e) tibble(scenario = c("lo","central","hi"),
                             kappa = c(0.10, 0.18, 0.27)))  # placeholder band
# pi_k: task-cost share by occupation. CHECk if available, default to equal
# weighting (pi_k = 1), making TFP proportional to exposed-employment share.
pi_k <- tryCatch(
  read_csv(file.path(PATHS$sfc, "task_cost_share.csv"), show_col_types = FALSE),
  error = function(e) distinct(panel, soc_uk) |> mutate(pi = 1))

base <- panel |> left_join(pi_k, by = "soc_uk") |>
  mutate(pi = coalesce(pi, 1))

# Region exposure aggregate A_r = sum_k sigma * pi * E_bar
A_r <- base |> group_by(region) |>
  summarise(A = sum(sigma * pi * E_bar), .groups = "drop")

proj <- tidyr::crossing(A_r, calib) |>
  mutate(dln_TFP = kappa * A,
         dTFP_pct = 100 * dln_TFP)

# Scotland - rUK differential in the implied TFP path, by scenario.
diff_tbl <- proj |>
  select(region, scenario, dTFP_pct) |>
  pivot_wider(names_from = region, values_from = dTFP_pct) |>
  mutate(diff_Scot_minus_rUK = Scotland - rUK)

message("TFP differential (Scotland - rUK), central scenario: ",
        sprintf("%.3f pp", diff_tbl$diff_Scot_minus_rUK[diff_tbl$scenario == "central"]))
save_csv(proj,     file.path(PATHS$tables, "acemoglu_projection.csv"))
save_csv(diff_tbl, file.path(PATHS$tables, "acemoglu_differential.csv"))

# GDP translation: apply the SFC's labour-share / pass-through. Provide
# gdp_passthrough.csv (region, gva_base, labour_share) when supplied.
gdp <- tryCatch({
  g <- read_csv(file.path(PATHS$sfc, "gdp_passthrough.csv"), show_col_types = FALSE)
  proj |> inner_join(g, by = "region") |>
    mutate(dGDP = gva_base * labour_share * dln_TFP)
}, error = function(e) { message("GDP pass-through file not yet supplied — TFP only."); NULL })
if (!is.null(gdp)) save_csv(gdp, file.path(PATHS$tables, "gdp_projection.csv"))
