# =====================================================================
# 11_calibration_experiment.R  —  Scoring-variance calibration
#
# RUNS AFTER THE FULL V0 SCORING (04 + 05): the sample is stratified on
# realised scores, and the V0 arm is reused from the full run rather
# than re-bought.
#
# Design (Table 1 ):
#   * 9 occupations per major group: 5 drawn at RANDOM (the estimation
#     stratum for sigma^2_g -- a purely boundary-proximate sample would
#     bias the noise estimate if ambiguity varies with score level) and
#     4 nearest the classification boundary (held out to validate the
#     CSS out of sample).
#   * Noise variants {V0, V2, V5, V6}: meaning-preserving perturbations
#     only. sigma^2_g comes from these, on the random stratum.
#   * Sensitivity variants {V1, V3}: these change the question (horizon;
#     rubric form), not its phrasing. Their occupation-level shifts are
#     summarised separately and NEVER pooled into the noise.
#   * Cross-model contrasts come from script 14's full re-scores; no
#     cross-model arm is needed here.
#
# Outputs:
#   sigma2_j.csv, sigma2_g.csv          occupation-composite noise
#   sigma2_task_g.csv                   task-level noise (sub & comp) -> CSS
#   calib_sensitivity.csv               V1-V0 and V3-V0 shift summaries
#   calib_fliprate.csv                  class stability across noise variants
#
# Transport: Anthropic Batch API throughout (all scored variants run on
# model M), resumable via batch_run().
# =====================================================================
source(here::here("R", "03_score.R"))
source(here::here("R", "05_aggregate_occupation.R"))

task_df <- read_csv(file.path(PATHS$cache, "task_df.csv"), show_col_types = FALSE)
occ0    <- read_csv(require_file(
             file.path(PATHS$cache, "occupation_scores_V0.csv"),
             "run 04_run_full_scoring.R and 05_aggregate_occupation.R first"),
           show_col_types = FALSE)
thr <- THRESHOLDS$central

# --- Stratified sample: random + near-boundary within major group ------
# Boundary proximity is measured against the classification actually
# used: distance of Exp_j to the materiality cut and of Sub_j to the
# majority cut within the exposed mass.
set.seed(SEED)
occ0 <- occ0 |>
  mutate(major = major_group(onet_soc_code),
         near_boundary = pmin(abs(Exp_j - EXP_MATERIALITY),
                              abs(Sub_j - 0.5 * Exp_j)))

near <- occ0 |> group_by(major) |>
  arrange(near_boundary, .by_group = TRUE) |>
  slice_head(n = CALIB$n_near_boundary) |>
  ungroup() |> mutate(stratum = "near_boundary")

rand <- occ0 |> anti_join(near, by = "onet_soc_code") |>
  group_by(major) |> slice_sample(n = CALIB$n_random) |>
  ungroup() |> mutate(stratum = "random")

samp <- bind_rows(rand, near)
message("calibration occupations: ", nrow(samp), " (",
        sum(samp$stratum == "random"), " random / ",
        sum(samp$stratum == "near_boundary"), " near-boundary) across ",
        n_distinct(samp$major), " major groups")
save_csv(samp |> select(onet_soc_code, major, stratum),
         file.path(PATHS$cache, "calib_sample.csv"))

calib_tasks <- task_df |> semi_join(samp, by = "onet_soc_code")

# --- Score V1, V2, V3, V5, V6
scores_v0 <- read_scores(file.path(PATHS$scores, "task_scores_V0.csv")) |>
  semi_join(samp, by = "onet_soc_code")

calib_scores <- c(
  list(V0 = scores_v0),
  map(set_names(CALIB$score_variants), function(v) {
    message("scoring calibration under ", v, " (", MODELS$M$id, ", batch) ...")
    batch_run(calib_tasks, v, paste0("calib_", v))
  })
)


occ_by_variant <- imap_dfr(calib_scores, function(s, v) {
  aggregate_occupation(s, calib_tasks, thr) |>
    transmute(onet_soc_code, variant = v, s_jk = E_j,
              classification)
}) |> left_join(samp |> select(onet_soc_code, stratum), by = "onet_soc_code")

# Occupation-composite noise: sigma^2_j, sigma^2_g
wm <- occ_by_variant |>
  filter(variant %in% CALIB$noise_variants, stratum == "random")
sigma2_j <- wm |> group_by(onet_soc_code) |>
  summarise(sigma2_j = var(s_jk), s_bar = mean(s_jk), .groups = "drop") |>
  mutate(major = major_group(onet_soc_code))
sigma2_g <- sigma2_j |> group_by(major) |>
  summarise(sigma2_g = mean(sigma2_j, na.rm = TRUE), n = n(), .groups = "drop")
message("within-model sigma_g (sd points) by major group:\n",
        paste(sigma2_g$major, round(sqrt(sigma2_g$sigma2_g), 1), collapse = "  "))

# Task-level noise (feeds the task-level CSS in 13
task_noise <- imap_dfr(calib_scores[CALIB$noise_variants], function(s, v)
                s |> mutate(variant = v)) |>
  semi_join(samp |> filter(stratum == "random"), by = "onet_soc_code") |>
  group_by(onet_soc_code, task_id) |>
  summarise(v_sub = var(sub), v_comp = var(comp), .groups = "drop") |>
  mutate(major = major_group(onet_soc_code))
sigma2_task_g <- task_noise |> group_by(major) |>
  summarise(sigma2_sub_g  = mean(v_sub,  na.rm = TRUE),
            sigma2_comp_g = mean(v_comp, na.rm = TRUE),
            n_tasks = n(), .groups = "drop")

# Estimand sensitivity: V1 (horizon) and V3 (rubric form)
wide_s <- occ_by_variant |>
  filter(variant %in% c("V0", CALIB$sensitivity_variants)) |>
  select(onet_soc_code, variant, s_jk) |>
  pivot_wider(names_from = variant, values_from = s_jk)
sens <- map_dfr(CALIB$sensitivity_variants, function(v) {
  d <- wide_s[[v]] - wide_s$V0
  tibble(variant = v,
         mean_shift = mean(d, na.rm = TRUE),
         sd_shift   = sd(d,   na.rm = TRUE),
         cor_with_V0 = cor(wide_s[[v]], wide_s$V0, use = "complete.obs"))
})
message("estimand sensitivity (shift vs V0): ",
        paste(sens$variant, sprintf("mean %.1f sd %.1f", sens$mean_shift, sens$sd_shift),
              collapse = " | "))

# Classification flip rate across noise variants
# Empirical stability check the CSS must predict: an occupation "flips"
# if its class is not constant across {V0, V2, V5, V6}.
flips <- occ_by_variant |>
  filter(variant %in% CALIB$noise_variants) |>
  group_by(onet_soc_code, stratum) |>
  summarise(n_classes = n_distinct(classification),
            flipped = n_classes > 1, .groups = "drop")
message("classification flip rate across noise variants: ",
        sprintf("%.1f%% overall | %.1f%% near-boundary | %.1f%% random",
                100 * mean(flips$flipped),
                100 * mean(flips$flipped[flips$stratum == "near_boundary"]),
                100 * mean(flips$flipped[flips$stratum == "random"])))

save_csv(sigma2_j,      file.path(PATHS$cache, "sigma2_j.csv"))
save_csv(sigma2_g,      file.path(PATHS$cache, "sigma2_g.csv"))
save_csv(sigma2_task_g, file.path(PATHS$cache, "sigma2_task_g.csv"))
save_csv(sens,          file.path(PATHS$tables, "calib_sensitivity.csv"))
save_csv(flips,         file.path(PATHS$cache, "calib_fliprate.csv"))
