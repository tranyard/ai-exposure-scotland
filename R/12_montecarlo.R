# =====================================================================
# 12_montecarlo.R  —  Uncertainty propagation (Plan §2.6).
# Draws R perturbed occupation scores from the within-model error model,
# re-aggregates to UK SOC, re-classifies, re-weights by APS employment,
# and returns credible intervals for E_hat_r and Delta E_hat. Includes
# the correlated-within-group robustness (rho grid).
# =====================================================================
source(here::here("R", "00_config.R"))

occ      <- read_csv(file.path(PATHS$cache, "occupation_scores_V0.csv"), show_col_types = FALSE)
sigma2_g <- read_csv(file.path(PATHS$cache, "sigma2_g.csv"), show_col_types = FALSE) |>
  mutate(major = as.character(major))
aps      <- read_csv(file.path(PATHS$cache, "aps_pool.csv"), show_col_types = FALSE) |>
  mutate(soc_uk = as.character(soc_uk))

# Attach group noise sd to each O*NET occupation (impute group mean).
sg_bar <- mean(sigma2_g$sigma2_g, na.rm = TRUE)
occ <- occ |> mutate(major = major_group(onet_soc_code)) |>
  left_join(sigma2_g |> select(major, sigma2_g), by = "major") |>
  mutate(sigma_g = sqrt(coalesce(sigma2_g, sg_bar)))

# Pre-load the direct crosswalk once (re-used every draw), reduced to the
# APS working level.
map3 <- read_uk_onet_map() |>
  mutate(soc_uk = substr(gsub("[^0-9]", "", soc_uk4), 1, APS$soc_level)) |>
  select(onet_soc_code, soc_uk) |> distinct()
thr <- THRESHOLDS$central

# One draw: perturb E_j -> map O*NET -> UK minor group -> region indices.
one_draw <- function(eps) {
  s <- pmin(pmax(occ$E_j + eps, 0), 100)
  d <- tibble(onet_soc_code = occ$onet_soc_code, E = s)
  uk <- map3 |> inner_join(d, by = "onet_soc_code") |>
        group_by(soc_uk) |> summarise(E_uk = mean(E), .groups = "drop") |>
        mutate(exposed = E_uk >= thr["comp"])
  idx <- aps |> inner_join(uk, by = "soc_uk") |> group_by(region) |>
        summarise(E_hat = sum(sigma * exposed), .groups = "drop")
  c(Scot = idx$E_hat[idx$region == "Scotland"], rUK = idx$E_hat[idx$region == "rUK"])
}

# Independent draws.
draw_eps_indep <- function() rnorm(nrow(occ), 0, occ$sigma_g)

# Correlated within major group: eps_g ~ N(0, sigma^2_g[(1-rho)I + rho 11']).
draw_eps_corr <- function(rho) {
  grp_shock <- tapply(seq_len(nrow(occ)), occ$major, function(i) rnorm(1))[occ$major]
  idio <- rnorm(nrow(occ))
  occ$sigma_g * (sqrt(rho) * grp_shock + sqrt(1 - rho) * idio)
}

run_mc <- function(rho = 0) {
  set.seed(SEED)
  draws <- map(seq_len(MONTECARLO$R), function(r) {
    eps <- if (rho == 0) draw_eps_indep() else draw_eps_corr(rho)
    one_draw(eps)
  })
  m <- do.call(rbind, draws) |> as_tibble()
  m <- m |> mutate(dE = Scot - rUK)
  tibble(
    rho = rho,
    quantity = c("E_Scotland","E_rUK","Delta_E"),
    mean = c(mean(m$Scot), mean(m$rUK), mean(m$dE)),
    sd   = c(sd(m$Scot), sd(m$rUK), sd(m$dE)),
    p2.5 = c(quantile(m$Scot,.025), quantile(m$rUK,.025), quantile(m$dE,.025)),
    p97.5= c(quantile(m$Scot,.975), quantile(m$rUK,.975), quantile(m$dE,.975)),
    p_sign = c(NA, NA, mean(sign(m$dE) == sign(mean(m$dE))))
  )
}

mc <- map_dfr(MONTECARLO$rho_grid, run_mc)
print(mc, n = Inf)
save_csv(mc, file.path(PATHS$tables, "montecarlo_intervals.csv"))
message("Delta E 95% CI (rho=0): [",
        sprintf("%.4f, %.4f", mc$p2.5[mc$quantity=="Delta_E" & mc$rho==0],
                mc$p97.5[mc$quantity=="Delta_E" & mc$rho==0]), "]")
