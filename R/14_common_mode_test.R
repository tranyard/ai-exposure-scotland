# =====================================================================
# 14_common_mode_test.R  —  Cross-model robustness (Plan §2.5, Cor. 1).
# Re-scores the full task set under the alternative model M', then:
#   * Proposition 1 check: the bias contaminates the LEVEL by ~mean bias
#     but the DIFFERENTIAL only by sum_k (sigma_Scot - sigma_rUK) b_k.
#   * Corollary 1: Delta E_bar is far more stable across M, M' than the
#     levels E_Scot, E_rUK individually.
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

# --- Full M' scoring (cached) ----------------------------------------
f_mp <- file.path(PATHS$scores, "task_scores_V4_full.csv")
if (file.exists(f_mp)) {
  scores_mp <- read_csv(f_mp, show_col_types = FALSE)
} else {
  message("scoring full task set under M' = ", MODELS$Mp$id, " ...")
  scores_mp <- score_sync(task_df, "V4")   # use batch_* if M' is Anthropic
  save_csv(scores_mp, f_mp)
}
scores_m  <- read_csv(file.path(PATHS$scores, "task_scores_V0.csv"), show_col_types = FALSE)

# --- Continuous UK SOC scores from an occupation table ---------------
uk_continuous <- function(occ) {
  map3 |>
    inner_join(occ |> mutate(onet_soc_code = as.character(onet_soc_code)) |>
                 select(onet_soc_code, E_j), by = "onet_soc_code") |>
    group_by(soc_uk) |> summarise(E_uk = mean(E_j), .groups = "drop")
}

occ_m  <- aggregate_occupation(scores_m,  task_df, thr)
occ_mp <- aggregate_occupation(scores_mp, task_df, thr)
uk_m   <- uk_continuous(occ_m)  |> rename(E_uk_M  = E_uk)
uk_mp  <- uk_continuous(occ_mp) |> rename(E_uk_Mp = E_uk)

# --- Region shares wide ----------------------------------------------
shares <- aps |> select(soc_uk, region, sigma) |>
  pivot_wider(names_from = region, values_from = sigma, values_fill = 0) |>
  mutate(dsigma = Scotland - rUK)            # sum_k dsigma = 0

merged <- shares |> inner_join(uk_m, by = "soc_uk") |>
  inner_join(uk_mp, by = "soc_uk") |>
  mutate(b = (E_uk_M - E_uk_Mp) / 100)       # cross-model bias, continuous scale

# --- Region indices under each model ---------------------------------
idx <- function(col) merged |>
  summarise(Scot = sum(Scotland * .data[[col]]/100),
            rUK  = sum(rUK      * .data[[col]]/100)) |>
  mutate(dE = Scot - rUK)
i_m  <- idx("E_uk_M");  i_mp <- idx("E_uk_Mp")

# --- Proposition 1: level vs differential contamination --------------
level_contam_Scot <- sum(merged$Scotland * merged$b)          # ~ mean bias
level_contam_rUK  <- sum(merged$rUK      * merged$b)
diff_contam       <- sum(merged$dsigma   * merged$b)          # the cancelling term

cmc <- tibble(
  quantity = c("E_bar_Scot", "E_bar_rUK", "Delta_E_bar"),
  under_M  = c(i_m$Scot,  i_m$rUK,  i_m$dE),
  under_Mp = c(i_mp$Scot, i_mp$rUK, i_mp$dE),
  cross_model_shift = c(i_m$Scot - i_mp$Scot, i_m$rUK - i_mp$rUK, i_m$dE - i_mp$dE)
)
print(cmc)
message(sprintf(
  "Prop 1: level contamination ~ %.4f (Scot), %.4f (rUK) | differential contamination = %.5f",
  level_contam_Scot, level_contam_rUK, diff_contam))
message(sprintf(
  "Cor 1: |cross-model shift| in levels = %.4f / %.4f vs in the gap = %.5f",
  abs(cmc$cross_model_shift[1]), abs(cmc$cross_model_shift[2]),
  abs(cmc$cross_model_shift[3])))

save_csv(cmc, file.path(PATHS$tables, "common_mode_test.csv"))
save_csv(merged |> select(soc_uk, dsigma, E_uk_M, E_uk_Mp, b),
         file.path(PATHS$cache, "cross_model_full.csv"))
