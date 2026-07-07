# =====================================================================
# 04_run_full_scoring.R  —  Score the full O*NET task set under the
# baseline model M, variant V0, via the Anthropic Batch API.
# Writes out/scores/task_scores_V0.csv.
#
# Resumable at every stage: the batch ID is persisted the moment the
# batch is accepted, so a dropped session resumes polling rather than
# resubmitting (batch results stay retrievable for 29 days). Parse
# failures are retried once synchronously; any residue is written to a
# diagnostics file and reported, and is dropped with weight
# renormalisation downstream in 05 (never silently propagated).
#
# Ballpark cost (Sonnet 4.6, Batch API): ~19,500 tasks x ~500 tokens in
# / ~40 out. Confirm live per-token prices on the console before
# submitting.
# =====================================================================
source(here::here("R", "03_score.R"))

task_df <- read_csv(require_file(file.path(PATHS$cache, "task_df.csv"),
                                 "run 01_prepare_onet.R first"),
                    show_col_types = FALSE)

out_file <- file.path(PATHS$scores, "task_scores_V0.csv")

message("scoring ", nrow(task_df), " tasks under ", MODELS$M$id, " (V0) ...")
scores <- batch_run(task_df, "V0", "V0")   # read / resume / submit as needed

# --- Validation: retry any parse failures once, synchronously ---------
bad <- scores |> filter(is.na(sub) | is.na(comp))
if (nrow(bad)) {
  message("re-trying ", nrow(bad), " failed parses synchronously ...")
  retry_tasks <- task_df |> semi_join(bad, by = c("onet_soc_code", "task_id"))
  fixed <- score_sync(retry_tasks, "V0")
  scores <- scores |> anti_join(bad, by = c("onet_soc_code", "task_id")) |>
    bind_rows(fixed)
  save_csv(scores, out_file)
}

# --- Diagnostics: any residual failures are logged, not hidden --------
still_bad <- scores |> filter(is.na(sub) | is.na(comp))
if (nrow(still_bad)) {
  save_csv(still_bad |> select(onet_soc_code, task_id),
           file.path(PATHS$cache, "parse_failures_V0.csv"))
  message("WARNING: ", nrow(still_bad), " tasks (",
          sprintf("%.2f%%", 100 * nrow(still_bad) / nrow(scores)),
          ") remain unparsed after retry. They are dropped with weight ",
          "renormalisation in 05; report the count in Appendix B. ",
          "If this exceeds ~2%, tighten the JSON instruction and re-run.")
} else {
  message("all ", nrow(scores), " tasks parsed successfully.")
}
