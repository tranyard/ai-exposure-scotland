# Uncertainty propagation for the regional differential.
#
# Three components, reported separately and composed:
#   scoring   within-model prompt noise (sigma^2_g). Perfectly
#             common-mode across regions by construction, so this is the
#             numerical illustration of Proposition 1, not an interval.
#   sampling  APS person-bootstrap of the employment shares (from 07).
#   composed  both at once: the interval for Delta E_bar.
# The rho grid applies to the scoring component.
source(here::here("R", "00_config.R"))

occ      <- read_csv(file.path(PATHS$cache, "occupation_scores_V0.csv"), show_col_types = FALSE)
sigma2_g <- read_csv(file.path(PATHS$cache, "sigma2_g.csv"), show_col_types = FALSE) |>
  mutate(major = as.character(major))
aps      <- read_csv(file.path(PATHS$cache, "aps_pool.csv"), show_col_types = FALSE) |>
  mutate(soc_uk = as.character(soc_uk))
boot_f   <- file.path(PATHS$cache, "aps_sigma_boot.csv")
has_boot <- file.exists(boot_f)
if (!has_boot)
  warning("aps_sigma_boot.csv not found - falling back to scoring-only propagation.",
          call. = FALSE)

sg_bar <- mean(sigma2_g$sigma2_g, na.rm = TRUE)
occ <- occ |> mutate(major = major_group(onet_soc_code)) |>
  left_join(sigma2_g |> select(major, sigma2_g), by = "major") |>
  mutate(sigma_g = sqrt(coalesce(sigma2_g, sg_bar)))

# Pre-built aggregation structure mirroring 06 exactly, for speed.
map <- read_uk_onet_map()
if (!"weight" %in% names(map)) map$weight <- 1
agg_pre <- map |>
  mutate(onet_soc_code = as.character(onet_soc_code)) |>
  inner_join(tibble(onet_soc_code = as.character(occ$onet_soc_code),
                    occ_idx = seq_len(nrow(occ))),
             by = "onet_soc_code") |>
  group_by(soc_uk4) |> mutate(wnorm = weight / sum(weight)) |> ungroup() |>
  mutate(soc_uk = substr(gsub("[^0-9]", "", soc_uk4), 1, APS$soc_level))

uk3_draw <- function(E) {
  agg_pre |>
    mutate(E = E[occ_idx]) |>
    group_by(soc_uk, soc_uk4) |>
    summarise(E4 = sum(wnorm * E), .groups = "drop") |>
    group_by(soc_uk) |>
    summarise(E_uk = mean(E4), .groups = "drop")
}

shares_pt <- aps |> select(soc_uk, region, sigma) |>
  pivot_wider(names_from = region, values_from = sigma,
              values_fill = list(sigma = 0))

if (has_boot) {
  boot <- read_csv(boot_f, show_col_types = FALSE) |>
    mutate(soc_uk = as.character(soc_uk))
  B <- max(boot$rep)
  shares_boot <- boot |>
    pivot_wider(names_from = region, values_from = sigma_b,
                values_fill = list(sigma_b = 0)) |>
    group_split(rep, .keep = FALSE)
}

gap_from <- function(uk, sh) {
  m <- inner_join(sh, uk, by = "soc_uk")
  c(Scot = sum(m$Scotland * m$E_uk / 100),
    rUK  = sum(m$rUK      * m$E_uk / 100))
}

# The unperturbed draw must reproduce the headline gap.
pt <- gap_from(uk3_draw(occ$E_j), shares_pt)
hl_f <- table_csv("region_indices_continuous")
if (file.exists(hl_f)) {
  hl <- read_csv(hl_f, show_col_types = FALSE) |> filter(operator == "max")
  stopifnot(abs((pt["Scot"] - pt["rUK"]) - hl$gap) < 1e-8)
}

draw_eps_indep <- function() rnorm(nrow(occ), 0, occ$sigma_g)
draw_eps_corr <- function(rho) {
  grp_shock <- tapply(seq_len(nrow(occ)), occ$major, function(i) rnorm(1))[occ$major]
  idio <- rnorm(nrow(occ))
  occ$sigma_g * (sqrt(rho) * grp_shock + sqrt(1 - rho) * idio)
}

uk_pt <- uk3_draw(occ$E_j)

run_mc <- function(component, rho = 0) {
  set.seed(SEED + round(100 * rho) + match(component, c("scoring", "sampling", "composed")))
  draws <- map(seq_len(MONTECARLO$R), function(r) {
    uk <- if (component == "sampling") uk_pt else {
      eps <- if (rho == 0) draw_eps_indep() else draw_eps_corr(rho)
      uk3_draw(pmin(pmax(occ$E_j + eps, 0), 100))
    }
    sh <- if (component == "scoring" || !has_boot) shares_pt
          else shares_boot[[((r - 1) %% B) + 1]]
    v <- gap_from(uk, sh)
    c(v, dE = unname(v["Scot"] - v["rUK"]))
  })
  m <- as_tibble(do.call(rbind, draws))
  names(m) <- c("Scot", "rUK", "dE")
  tibble(
    component = component, rho = rho,
    quantity = c("Ebar_Scotland", "Ebar_rUK", "Delta_Ebar"),
    mean  = c(mean(m$Scot), mean(m$rUK), mean(m$dE)),
    sd    = c(sd(m$Scot),   sd(m$rUK),   sd(m$dE)),
    p2.5  = c(quantile(m$Scot, .025), quantile(m$rUK, .025), quantile(m$dE, .025)),
    p97.5 = c(quantile(m$Scot, .975), quantile(m$rUK, .975), quantile(m$dE, .975)),
    p_sign = c(NA, NA, mean(sign(m$dE) == sign(mean(m$dE))))
  )
}

specs <- bind_rows(
  tibble(component = "scoring",  rho = MONTECARLO$rho_grid),
  if (has_boot) tibble(component = "sampling", rho = 0),
  if (has_boot) tibble(component = "composed", rho = MONTECARLO$rho_grid)
)
mc <- pmap_dfr(specs, \(component, rho) run_mc(component, rho))
save_csv(mc, file.path(PATHS$tables, "montecarlo_intervals.csv"))

d0 <- mc |> filter(quantity == "Delta_Ebar")
walk(seq_len(nrow(d0)), \(i) message(sprintf(
  "Delta E_bar 95%% CI [%s, rho=%.1f]: [%.4f, %.4f] (sd %.5f)",
  d0$component[i], d0$rho[i], d0$p2.5[i], d0$p97.5[i], d0$sd[i])))
