# Score the full O*NET task set under model M, variant V0, via the
# Anthropic Batch API. Writes out/scores/task_scores_V0.csv.
source(here::here("R", "03_score.R"))

task_df <- read_csv(require_file(file.path(PATHS$cache, "task_df.csv"),
                                 "run 01_prepare_onet.R first"),
                    show_col_types = FALSE)

out_file <- file.path(PATHS$scores, "task_scores_V0.csv")

scores <- batch_run(task_df, "V0", "V0")

# Retry parse failures once, synchronously.
bad <- scores |> filter(is.na(sub) | is.na(comp))
if (nrow(bad)) {
  retry_tasks <- task_df |> semi_join(bad, by = c("onet_soc_code", "task_id"))
  fixed <- score_sync(retry_tasks, "V0")
  scores <- scores |> anti_join(bad, by = c("onet_soc_code", "task_id")) |>
    bind_rows(fixed)
  save_csv(scores, out_file)
}

# Residual failures are logged and dropped with weight renormalisation in 05.
still_bad <- scores |> filter(is.na(sub) | is.na(comp))
if (nrow(still_bad)) {
  save_csv(still_bad |> select(onet_soc_code, task_id),
           file.path(PATHS$cache, "parse_failures_V0.csv"))
  message(nrow(still_bad), " of ", nrow(scores), " tasks (",
          sprintf("%.2f%%", 100 * nrow(still_bad) / nrow(scores)),
          ") remain unparsed after retry.")
}
