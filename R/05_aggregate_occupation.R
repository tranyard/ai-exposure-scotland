source(here::here("R", "00_config.R"))

aggregate_occupation <- function(scores, task_df, thr = THRESHOLDS$central) {
  d <- scores |>
    inner_join(select(task_df, onet_soc_code, task_id, importance),
               by = c("onet_soc_code", "task_id")) |>
    mutate(scored = !is.na(sub) & !is.na(comp))

  lost <- d |>
    group_by(onet_soc_code) |>
    summarise(lost = sum(importance[!scored]) / sum(importance),
              n_dropped = sum(!scored), .groups = "drop") |>
    filter(n_dropped > 0)
  if (nrow(lost)) {
    save_csv(lost, file.path(PATHS$cache, "task_score_coverage.csv"))
    if (max(lost$lost) > 0.10)
      warning(sprintf("an occupation lost %.0f%% of its importance mass to parse failures",
                      100 * max(lost$lost)), call. = FALSE)
  }

  d |>
    filter(scored) |>
    group_by(onet_soc_code) |>
    mutate(w = importance / sum(importance)) |>
    summarise(
      E_j      = sum(w * OPERATORS$max(sub, comp)),   # headline
      E_sub_j  = sum(w * OPERATORS$sub(sub, comp)),
      E_comp_j = sum(w * comp),
      E_sat_j  = sum(w * OPERATORS$sat(sub, comp)),
      Sub_j    = sum(w * (sub >= thr["sub"])),
      Comp_j   = sum(w * (comp >= thr["comp"] & sub < thr["sub"])),
      .groups  = "drop"
    ) |>
    mutate(Exp_j = Sub_j + Comp_j,
           classification = classify_occ(Sub_j, Comp_j))
}

if (file.exists(file.path(PATHS$scores, "task_scores_V0.csv"))) {
  task_df <- read_csv(file.path(PATHS$cache, "task_df.csv"), show_col_types = FALSE)
  scores  <- read_csv(file.path(PATHS$scores, "task_scores_V0.csv"), show_col_types = FALSE)
  for (set in names(THRESHOLDS)) {
    occ    <- aggregate_occupation(scores, task_df, THRESHOLDS[[set]])
    suffix <- if (set == "central") "" else paste0("_", set)
    save_csv(occ, file.path(PATHS$cache, paste0("occupation_scores_V0", suffix, ".csv")))
  }
  message(nrow(occ), " occupations | central class: ",
          paste(names(table(occ$classification)), table(occ$classification),
                sep = "=", collapse = " "))
}
