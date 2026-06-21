# =====================================================================
# 05_aggregate_occupation.R  —  Occupation-level exposure (Plan §1.1).
# Importance-weighted composite E_j = sum_t w_jt * max(sub, comp),
# mode-specific scores, threshold shares, and the OBR classification rule.
# Generic over variant so it serves both V0 and the calibration variants.
# =====================================================================
source(here::here("R", "00_config.R"))

aggregate_occupation <- function(scores, task_df, thr = THRESHOLDS$central) {
  d <- scores |>
    inner_join(select(task_df, onet_soc_code, task_id, importance),
               by = c("onet_soc_code", "task_id")) |>
    mutate(s_task = pmax(sub, comp, na.rm = TRUE))

  # Normalised importance weights within occupation (sum to 1).
  d <- d |> group_by(onet_soc_code) |>
    mutate(w = importance / sum(importance)) |> ungroup()

  occ <- d |> group_by(onet_soc_code) |> summarise(
    E_j      = sum(w * s_task),
    E_sub_j  = sum(w * sub),
    E_comp_j = sum(w * comp),
    # Threshold task shares (Plan §1.1 / framework §4.3)
    Sub_j  = sum(w * (sub >= thr["sub"])),
    Comp_j = sum(w * (sub >= thr["comp"] & sub < thr["sub"])),
    .groups = "drop"
  ) |> mutate(
    Exp_j = Sub_j + Comp_j,
    classification = case_when(
      Exp_j <= 1e-9                      ~ "Unexposed",
      Sub_j > 0.5 * Exp_j                ~ "Substituted",
      TRUE                               ~ "Complemented"
    )
  )
  occ
}

# --- Run for the baseline V0 (only when the V0 scores exist; safe to
#     source this file purely for the function above) ------------------
if (file.exists(file.path(PATHS$scores, "task_scores_V0.csv"))) {
  task_df <- read_csv(file.path(PATHS$cache, "task_df.csv"), show_col_types = FALSE)
  scores  <- read_csv(file.path(PATHS$scores, "task_scores_V0.csv"), show_col_types = FALSE)
  occ <- aggregate_occupation(scores, task_df)
  message("classified occupations: ",
          paste(names(table(occ$classification)), table(occ$classification),
                sep = "=", collapse = "  "))
  save_csv(occ, file.path(PATHS$cache, "occupation_scores_V0.csv"))
}
