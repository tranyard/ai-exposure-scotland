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
           comp = classification == "Complemented")
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
save_csv(ri, file.path(PATHS$tables, "region_indices.csv"))

gap <- ri |> summarise(
  dE_hat = E_hat[region == "Scotland"] - E_hat[region == "rUK"],
  dE_bar = E_bar[region == "Scotland"] - E_bar[region == "rUK"])
message(sprintf("dE_hat = %.4f | dE_bar = %.4f", gap$dE_hat, gap$dE_bar))

# Shift-share: the within term is zero by construction.
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

# Operator robustness: continuous gap under max, sub-only, saturating.
ops <- c(max = "E_uk", sub = "E_uk_sub", sat = "E_uk_sat")
op_gap <- imap_dfr(ops, \(col, nm)
  panel |> group_by(region) |>
    summarise(idx = sum(sigma * .data[[col]] / 100), .groups = "drop") |>
    pivot_wider(names_from = region, values_from = idx) |>
    transmute(operator = nm, Scotland, rUK, gap = Scotland - rUK))
save_csv(op_gap, file.path(PATHS$tables, "operator_robustness.csv"))
message("operator gaps: ",
        paste(op_gap$operator, sprintf("%.4f", op_gap$gap), sep = "=", collapse = " "))

# Threshold sensitivity of the classified gap.
thr_sens <- map_dfr(names(THRESHOLDS), \(set)
  region_indices(build_panel(set)) |>
    select(region, E_hat) |>
    pivot_wider(names_from = region, values_from = E_hat) |>
    transmute(thr_set = set, dE_hat = Scotland - rUK))
save_csv(thr_sens, file.path(PATHS$tables, "threshold_sensitivity.csv"))

# Sector exposure rates, each region separately (OBR chart idiom).
sector <- panel |>
  mutate(major = major_group(soc_uk)) |>
  group_by(region, major) |>
  summarise(E_rs = sum(l_pool * exposed) / sum(l_pool), emp = sum(l_pool), .groups = "drop")
save_csv(sector, file.path(PATHS$tables, "sector_exposure.csv"))

save_csv(panel, file.path(PATHS$cache, "region_panel.csv"))
