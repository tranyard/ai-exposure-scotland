# =====================================================================
# 11_calibration_experiment.R  —  Scoring-variance calibration
# Draws a stratified sample of occupations, scores their tasks under the
# five variants, aggregates to occupation composites, and estimates:
#   * within-model variance sigma^2_g  (from V0-V3 only)
#   * cross-model bias difference b^M_j - b^M'_j  (from V0 vs V4)
# This is the cheap rehearsal of the full run (~450 calls, < £5).
# =====================================================================
source(here::here("R", "03_score.R"))
source(here::here("R", "05_aggregate_occupation.R"))

task_df <- read_csv(file.path(PATHS$cache, "task_df.csv"), show_col_types = FALSE)
occ0    <- read_csv(file.path(PATHS$cache, "occupation_scores_V0.csv"), show_col_types = FALSE)

# --- Stratified sample: major group x threshold proximity ------------
thr <- THRESHOLDS$central
samp <- occ0 |>
  mutate(major = major_group(onet_soc_code),
         near_thr = pmin(abs(E_j - thr["sub"]), abs(E_j - thr["comp"]))) |>
  group_by(major) |>
  arrange(near_thr, .by_group = TRUE) |>     # oversample near thresholds
  slice_head(n = CALIB$per_major_group) |>
  ungroup()
message("calibration occupations: ", nrow(samp), " across ",
        n_distinct(samp$major), " major groups")

calib_tasks <- task_df |> semi_join(samp, by = "onet_soc_code")

# --- Score under each variant
score_variant <- function(v) {
  f <- file.path(PATHS$scores, paste0("calib_scores_", v, ".csv"))
  if (file.exists(f)) return(read_csv(f, show_col_types = FALSE))
  message("scoring calibration under ", v, " (", variant_spec(v)$model$id, ") ...")
  s <- score_sync(calib_tasks, v); save_csv(s, f); s
}
calib_scores <- map(set_names(CALIB$variants), score_variant)

# --- Occupation composites per variant -------------------------------
occ_by_variant <- imap_dfr(calib_scores, function(s, v) {
  aggregate_occupation(s, calib_tasks, thr) |>
    transmute(onet_soc_code, variant = v, s_jk = E_j)
})

# --- Within-model variance sigma^2_j and sigma^2_g (V0-V3) -----------
wm <- occ_by_variant |> filter(variant %in% CALIB$within_model_variants)
sigma2_j <- wm |> group_by(onet_soc_code) |>
  summarise(sigma2_j = var(s_jk), s_bar = mean(s_jk), .groups = "drop") |>
  mutate(major = major_group(onet_soc_code))
sigma2_g <- sigma2_j |> group_by(major) |>
  summarise(sigma2_g = mean(sigma2_j, na.rm = TRUE), n = n(), .groups = "drop")
message("within-model sigma_g (sd points) by major group:\n",
        paste(sigma2_g$major, round(sqrt(sigma2_g$sigma2_g), 1), collapse = "  "))

# --- Cross-model bias difference b^M - b^M' (V0 vs V4) ---------------
cross <- occ_by_variant |>
  filter(variant %in% c("V0", "V4")) |>
  pivot_wider(names_from = variant, values_from = s_jk) |>
  mutate(b_diff = V0 - V4)   # occupation-level model bias difference
message(sprintf("cross-model level shift: mean(b_diff) = %.1f, sd = %.1f",
                mean(cross$b_diff, na.rm = TRUE), sd(cross$b_diff, na.rm = TRUE)))

save_csv(sigma2_j, file.path(PATHS$cache, "sigma2_j.csv"))
save_csv(sigma2_g, file.path(PATHS$cache, "sigma2_g.csv"))
save_csv(cross,    file.path(PATHS$cache, "cross_model_bias_calib.csv"))

# --- (Optional) secondary analysis: variance vs info environment -----
# Provide info_env.csv (onet_soc_code, log_volume, contested) to run eq. 3.
#info_f <- file.path(PATHS$sfc, "info_env.csv")
#if (file.exists(info_f)) {
#  reg <- sigma2_j |> inner_join(read_csv(info_f, show_col_types = FALSE), by = "onet_soc_code")
#  print(summary(lm(sigma2_j ~ log_volume + contested, data = reg)))
#}
