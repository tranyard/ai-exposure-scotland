# =====================================================================
# 08_region_indices_shiftshare.R  —  Region indices + shift-share
# Builds E_hat_r, S_hat_r, C_hat_r, continuous E_bar_r, the gap Delta E_hat,
# and decomposes it. The within-occupation effect should be zero by
# construction (same scores both regions)
# =====================================================================
source(here::here("R", "00_config.R"))

uk   <- read_csv(file.path(PATHS$cache, "uk_soc3_scores.csv"), show_col_types = FALSE)
aps  <- read_csv(file.path(PATHS$cache, "aps_pool.csv"), show_col_types = FALSE)

panel <- aps |>
  inner_join(uk, by = "soc_uk") |>
  mutate(exposed = classification %in% c("Substituted", "Complemented"),
         sub = classification == "Substituted",
         comp = classification == "Complemented")

# --- Region-level indices (eq. 1A) ------------------------------------
region_idx <- panel |> group_by(region) |> summarise(
  E_hat = sum(sigma * exposed),
  S_hat = sum(sigma * sub),
  C_hat = sum(sigma * comp),
  E_bar = sum(sigma * E_uk / 100),
  .groups = "drop")
save_csv(region_idx, file.path(PATHS$tables, "region_indices.csv"))

gap <- region_idx |> summarise(
  dE_hat = E_hat[region == "Scotland"] - E_hat[region == "rUK"],
  dE_bar = E_bar[region == "Scotland"] - E_bar[region == "rUK"])
message(sprintf("Delta E_hat = %.4f | Delta E_bar = %.4f", gap$dE_hat, gap$dE_bar))

# --- Shift-share decomposition (eq. 2A) -------------------------------
wide <- panel |>
  select(soc_uk, region, sigma, exposed) |>
  pivot_wider(names_from = region, values_from = c(sigma, exposed),
              values_fill = list(sigma = 0)) |>
  mutate(exposed_rUK = coalesce(exposed_rUK, exposed_Scotland),
         exposed_Scotland = coalesce(exposed_Scotland, exposed_rUK))

shiftshare <- wide |> summarise(
  composition = sum((sigma_Scotland - sigma_rUK) * exposed_rUK),
  within      = sum(sigma_rUK * (exposed_Scotland - exposed_rUK)),  # == 0 by construction
  total       = composition + within)
message(sprintf("composition = %.4f | within = %.4g (zero by construction) | total = %.4f",
                shiftshare$composition, shiftshare$within, shiftshare$total))
save_csv(shiftshare, file.path(PATHS$tables, "shiftshare.csv"))

# --- Sector-level exposure rates (Chart XB.3 equivalent) --------------
sector <- panel |>
  mutate(major = major_group(soc_uk)) |>
  group_by(region, major) |>
  summarise(E_rs = sum(l_pool * exposed) / sum(l_pool), emp = sum(l_pool), .groups = "drop")
save_csv(sector, file.path(PATHS$tables, "sector_exposure.csv"))

save_csv(panel, file.path(PATHS$cache, "region_panel.csv"))
