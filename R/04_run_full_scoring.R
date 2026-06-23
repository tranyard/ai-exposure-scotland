# =====================================================================
# 04_run_full_scoring.R  —  Score the full O*NET task set under the
# baseline model M, variant V0
# Uses the Anthropic Batch API. Writes out/scores/task_scores_V0.csv.
#
# Ballpark cost: ~19,500 tasks x ~430 tokens in / ~40 out. Confirm the
# estimate before submitting; the calibration run (script 11) is the
# cheap rehearsal of this exact path.
# =====================================================================
source(here::here("R", "03_score.R"))

task_df <- read_csv(require_file(file.path(PATHS$cache, "task_df.csv"),
                                 "run 01_prepare_onet.R first"),
                    show_col_types = FALSE)

out_file <- file.path(PATHS$scores, "task_scores_V0.csv")
if (file.exists(out_file)) {
  message("already scored: ", out_file, " — delete to re-run.");
} else {
  message("submitting ", nrow(task_df), " tasks to the Batch API ...")
  bid <- batch_submit(task_df, "V0")
  scores <- batch_collect(bid, "V0")

  # Validation: re-try any parse failures once, synchronously.
  bad <- scores |> filter(is.na(sub) | is.na(comp))
  if (nrow(bad)) {
    message("re-trying ", nrow(bad), " failed parses synchronously ...")
    retry_tasks <- task_df |> semi_join(bad, by = c("onet_soc_code", "task_id"))
    fixed <- score_sync(retry_tasks, "V0")
    scores <- scores |> anti_join(bad, by = c("onet_soc_code", "task_id")) |>
      bind_rows(fixed)
  }
  save_csv(scores, out_file)
}
