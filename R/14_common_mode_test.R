# =====================================================================
# 14_common_mode_test.R  —  Cross-model robustness
# Re-scores the full task set under each alternative model (M' = GPT-4o,
# M'' = Haiku when configured), then:
#   * Proposition 1 check: the bias contaminates the LEVEL by ~mean bias
#     but the differential only by sum_k (sigma_Scot - sigma_rUK) b_k.
#   * Corollary 1: Delta E_bar is far more stable across models than the
#     levels E_Scot, E_rUK individually (common-mode cancellation), with
#     the empirical Cauchy-Schwarz bound reported alongside the realised
#     shift.
# Two contrasts (M vs M', M vs M'') rest the stability claim on more
# than a single comparison.
#
# Transport: Anthropic alternatives run through the resumable Batch API;
# the OpenAI arm runs synchronously with chunked checkpointing, so an
# interrupted multi-hour run resumes where it stopped.
# =====================================================================
source(here::here("R", "03_score.R"))
source(here::here("R", "05_aggregate_occupation.R"))

task_df <- read_csv(file.path(PATHS$cache, "task_df.csv"), show_col_types = FALSE)
aps     <- read_csv(file.path(PATHS$cache, "aps_pool.csv"), show_col_types = FALSE) |>
  mutate(soc_uk = as.character(soc_uk))
map3 <- read_uk_onet_map() |>
  mutate(soc_uk = substr(gsub("[^0-9]", "", soc_uk4), 1, APS$soc_level)) |>
  select(onet_soc_code, soc_uk) |> distinct()
thr <- THRESHOLDS$central

# --- Score the full set under each alternative model (cached/resumable)
alt_models <- compact(MODELS[c("Mp", "Mpp")])
score_alt <- function(m, name) {
  tag <- paste0("alt_", name)
  if (m$provider == "anthropic") batch_run(task_df, "V0", tag, model = m)
  else                           score_sync_chunked(task_df, "V0", tag, model = m)
}
scores_alt <- imap(alt_models, function(m, name) {
  message("scoring full task set under ", name, " = ", m$id, " ...")
  score_alt(m, name)
})
scores_m <- read_csv(file.path(PATHS$scores, "task_scores_V0.csv"), show_col_types = FALSE)

# --- Continuous UK SOC scores from an occupation table -----------------
uk_continuous <- function(occ) {
  map3 |>
    inner_join(occ |> mutate(onet_soc_code = as.character(onet_soc_code)) |>
                 select(onet_soc_code, E_j), by = "onet_soc_code") |>
    group_by(soc_uk) |> summarise(E_uk = mean(E_j), .groups = "drop")
}

uk_m <- uk_continuous(aggregate_occupation(scores_m, task_df, thr))

# --- Region shares wide -------------------------------------------------
shares <- aps |> select(soc_uk, region, sigma) |>
  pivot_wider(names_from = region, values_from = sigma, values_fill = 0) |>
  mutate(dsigma = Scotland - rUK)            # sum_k dsigma = 0

idx <- function(d, col) d |>
  summarise(Scot = sum(Scotland * .data[[col]] / 100),
            rUK  = sum(rUK      * .data[[col]] / 100)) |>
  mutate(dE = Scot - rUK)

# --- Proposition 1 / Corollary 1, one row per alternative model ---------
cmc <- imap_dfr(scores_alt, function(s_alt, name) {
  uk_alt <- uk_continuous(aggregate_occupation(s_alt, task_df, thr)) |>
    rename(E_uk_alt = E_uk)
  merged <- shares |>
    inner_join(uk_m,   by = "soc_uk") |>
    inner_join(uk_alt, by = "soc_uk") |>
    mutate(b = (E_uk - E_uk_alt) / 100)      # cross-model bias, continuous scale
  i_m   <- idx(merged, "E_uk")
  i_alt <- idx(merged, "E_uk_alt")
  b_dm  <- merged$b - mean(merged$b)         # Prop 1: only the demeaned part can survive
  cs_bound <- sqrt(sum(merged$dsigma^2)) * sqrt(sum(b_dm^2))
  save_csv(merged |> select(soc_uk, dsigma, E_uk, E_uk_alt, b),
           file.path(PATHS$cache, paste0("cross_model_full_", name, ".csv")))
  tibble(alt_model   = alt_models[[name]]$id,
         level_shift_Scot = i_m$Scot - i_alt$Scot,
         level_shift_rUK  = i_m$rUK  - i_alt$rUK,
         gap_shift        = i_m$dE   - i_alt$dE,
         cs_bound         = cs_bound,
         mean_bias        = mean(merged$b))
})
print(cmc)
walk(seq_len(nrow(cmc)), function(i) message(sprintf(
  "%s | levels shift %.4f / %.4f (~mean bias %.4f) | gap shift %.5f (C-S bound %.5f)",
  cmc$alt_model[i], abs(cmc$level_shift_Scot[i]), abs(cmc$level_shift_rUK[i]),
  cmc$mean_bias[i], abs(cmc$gap_shift[i]), cmc$cs_bound[i])))

save_csv(cmc, file.path(PATHS$tables, "common_mode_test.csv"))
