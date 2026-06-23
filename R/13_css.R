# =====================================================================
# 13_css.R  —  Classification stability scores
# CSS_j = Phi(|s_j - theta*_j| / sigma_g(j)), theta* = nearest threshold.
# Flags occupations with CSS < 0.75: these are where the
# common-mode cancellation argument is only approximate
# =====================================================================
source(here::here("R", "00_config.R"))

occ      <- read_csv(file.path(PATHS$cache, "occupation_scores_V0.csv"), show_col_types = FALSE)
sigma2_g <- read_csv(file.path(PATHS$cache, "sigma2_g.csv"), show_col_types = FALSE) |>
  mutate(major = as.character(major))
thr <- THRESHOLDS$central
sg_bar <- mean(sigma2_g$sigma2_g, na.rm = TRUE)   # fallback for unmatched groups

css <- occ |>
  mutate(major = major_group(onet_soc_code)) |>
  left_join(sigma2_g |> select(major, sigma2_g), by = "major") |>
  mutate(sigma_g = sqrt(coalesce(sigma2_g, sg_bar)),
         nearest_thr = if_else(abs(E_j - thr["sub"]) < abs(E_j - thr["comp"]),
                               thr["sub"], thr["comp"]),
         CSS = pnorm(abs(E_j - nearest_thr) / sigma_g),
         flag_uncertain = CSS < 0.75)

message("classification-uncertain occupations (CSS<0.75): ",
        sum(css$flag_uncertain), " of ", nrow(css))
save_csv(css |> select(onet_soc_code, E_j, classification, sigma_g, CSS, flag_uncertain),
         file.path(PATHS$tables, "classification_stability.csv"))
