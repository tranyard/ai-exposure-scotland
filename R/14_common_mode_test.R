# =====================================================================
# 14_common_mode_test.R  —  Cross-model robustness
#
# CHANGES:
#   1. Aggregation now uses the shared uk3_continuous() (mirrors the
#      script-06 crosswalk exactly), so the cross-model gaps are the
#      same estimand as the headline.
#   2. Arm control via env var: CM_ARM=Mpp (default, Haiku — the cheap
#      same-provider tier contrast, run this FIRST), CM_ARM=Mp (GPT-4o),
#      CM_ARM=BOTH. Spend on the OpenAI arm only once the Haiku arm has
#      confirmed the differential is stable.
#   3. Optional cost control: CM_SUBSAMPLE=N re-scores only the N O*NET
#      occupations carrying the most UK employment mass (via the
#      crosswalk); the gap is a weighted sum dominated by large cells,
#      so N ~ 300 typically covers >90% of employment. Coverage is
#      reported; both models' gaps are computed on the identical
#      covered set, so the gap SHIFT (the Corollary 1 object) is
#      internally consistent. Leave unset for the full re-score.
#
# Usage:
#   RUN_SCORING=1 CM_ARM=Mpp Rscript R/14_common_mode_test.R
#   RUN_SCORING=1 CM_ARM=Mpp CM_SUBSAMPLE=300 Rscript R/14_common_mode_test.R
#   RUN_SCORING=1 CM_ARM=Mp  Rscript R/14_common_mode_test.R   # later
# =====================================================================
source(here::here("R", "03_score.R"))
source(here::here("R", "03b_openai_batch.R"))
source(here::here("R", "05_aggregate_occupation.R"))

task_df <- read_csv(file.path(PATHS$cache, "task_df.csv"), show_col_types = FALSE)
aps     <- read_csv(file.path(PATHS$cache, "aps_pool.csv"), show_col_types = FALSE) |>
  mutate(soc_uk = as.character(soc_uk))
map <- read_uk_onet_map()
if (!"weight" %in% names(map)) map$weight <- 1
map3 <- map |>
  mutate(soc_uk = substr(gsub("[^0-9]", "", soc_uk4), 1, APS$soc_level)) |>
  select(onet_soc_code, soc_uk) |> distinct()
thr <- THRESHOLDS$central

# --- Arm selection ------------------------------------------------------
arm <- toupper(Sys.getenv("CM_ARM", "MPP"))
alt_keys <- switch(arm, MPP = "Mpp", MP = "Mp", BOTH = c("Mp", "Mpp"),
                   stop("CM_ARM must be Mpp, Mp or BOTH"))
alt_models <- compact(MODELS["Mpp"])   # Haiku only - change to "Mp" for Openai gpt 4o
message("cross-model arm(s): ",
        paste(map_chr(alt_models, "id"), collapse = ", "))

# --- Optional employment-mass subsample
n_sub <- as.integer(Sys.getenv("CM_SUBSAMPLE", "0"))
if (n_sub > 0) {
  # Distribute pooled UK employment to O*NET codes through the crosswalk
  # (each SOC3 cell's employment split equally over its mapped codes).
  emp3 <- aps |> group_by(soc_uk) |> summarise(l = sum(l_pool), .groups = "drop")
  occ_mass <- map3 |>
    inner_join(emp3, by = "soc_uk") |>
    group_by(soc_uk) |> mutate(l_share = l / n()) |> ungroup() |>
    group_by(onet_soc_code) |> summarise(mass = sum(l_share), .groups = "drop") |>
    arrange(desc(mass))
  keep <- occ_mass$onet_soc_code[seq_len(min(n_sub, nrow(occ_mass)))]
  coverage <- sum(occ_mass$mass[occ_mass$onet_soc_code %in% keep]) / sum(occ_mass$mass)
  task_df <- task_df |> filter(onet_soc_code %in% keep)
  message(sprintf("subsample: %d occupations | %.1f%% of UK employment mass | %d tasks",
                  length(keep), 100 * coverage, nrow(task_df)))
}

# --- Score the full/subsampled set under each alternative model
score_alt <- function(m, name) {
  tag <- paste0("alt_", name)
  if (m$provider == "anthropic") batch_run(task_df, "V0", tag, model = m)
  else                           openai_batch_run(task_df, "V0", tag, model = m)
}
scores_alt <- imap(alt_models, function(m, name) {
  message("scoring under ", name, " = ", m$id, " ...")
  score_alt(m, name)
})
scores_m <- read_csv(file.path(PATHS$scores, "task_scores_V0.csv"), show_col_types = FALSE) |>
  semi_join(task_df, by = c("onet_soc_code", "task_id"))

# --- Continuous UK SOC scores via the SHARED aggregation
uk_m <- uk3_continuous(aggregate_occupation(scores_m, task_df, thr),
                       cols = "E_j", map = map) |> rename(E_uk = E_j)

# --- Region shares wide --------------------------------------------------
shares <- aps |> select(soc_uk, region, sigma) |>
  pivot_wider(names_from = region, values_from = sigma, values_fill = 0) |>
  mutate(dsigma = Scotland - rUK)

idx <- function(d, col) d |>
  summarise(Scot = sum(Scotland * .data[[col]] / 100),
            rUK  = sum(rUK      * .data[[col]] / 100)) |>
  mutate(dE = Scot - rUK)

# --- Proposition 1 / Corollary 1, one row per alternative model ----------
cmc <- imap_dfr(scores_alt, function(s_alt, name) {
  uk_alt <- uk3_continuous(aggregate_occupation(s_alt, task_df, thr),
                           cols = "E_j", map = map) |> rename(E_uk_alt = E_j)
  merged <- shares |>
    inner_join(uk_m,   by = "soc_uk") |>
    inner_join(uk_alt, by = "soc_uk") |>
    mutate(b = (E_uk - E_uk_alt) / 100)
  emp_cov <- aps |> filter(soc_uk %in% merged$soc_uk) |>
    summarise(c = sum(l_pool)) |> pull(c) / sum(aps$l_pool)
  i_m   <- idx(merged, "E_uk")
  i_alt <- idx(merged, "E_uk_alt")
  b_dm  <- merged$b - mean(merged$b)
  cs_bound <- sqrt(sum(merged$dsigma^2)) * sqrt(sum(b_dm^2))
  save_csv(merged |> select(soc_uk, dsigma, E_uk, E_uk_alt, b),
           file.path(PATHS$cache, paste0("cross_model_full_", name, ".csv")))
  tibble(alt_model   = alt_models[[name]]$id,
         subsample   = n_sub,
         emp_coverage = emp_cov,
         level_shift_Scot = i_m$Scot - i_alt$Scot,
         level_shift_rUK  = i_m$rUK  - i_alt$rUK,
         gap_M            = i_m$dE,
         gap_alt          = i_alt$dE,
         gap_shift        = i_m$dE   - i_alt$dE,
         cs_bound         = cs_bound,
         mean_bias        = mean(merged$b))
})
print(cmc)
walk(seq_len(nrow(cmc)), function(i) message(sprintf(
  "%s | levels shift %.4f / %.4f (~mean bias %.4f) | gap %.5f -> %.5f (shift %.5f, C-S bound %.5f) | emp coverage %.0f%%",
  cmc$alt_model[i], abs(cmc$level_shift_Scot[i]), abs(cmc$level_shift_rUK[i]),
  cmc$mean_bias[i], cmc$gap_M[i], cmc$gap_alt[i], abs(cmc$gap_shift[i]),
  cmc$cs_bound[i], 100 * cmc$emp_coverage[i])))

out <- file.path(PATHS$tables,
                 if (n_sub > 0) sprintf("common_mode_test_top%d.csv", n_sub)
                 else "common_mode_test.csv")
save_csv(cmc, out)
