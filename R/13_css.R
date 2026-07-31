source(here::here("R", "00_config.R"))

task_df  <- read_csv(file.path(PATHS$cache, "task_df.csv"), show_col_types = FALSE)
scores   <- read_csv(file.path(PATHS$scores, "task_scores_V0.csv"), show_col_types = FALSE)
occ      <- read_csv(file.path(PATHS$cache, "occupation_scores_V0.csv"), show_col_types = FALSE)
sig_task <- read_csv(require_file(file.path(PATHS$cache, "sigma2_task_g.csv"),
                                  "run 11_calibration_experiment.R first"),
                     show_col_types = FALSE) |> mutate(major = as.character(major))
thr <- THRESHOLDS$central

ssub  <- mean(sig_task$sigma2_sub_g,  na.rm = TRUE)
scomp <- mean(sig_task$sigma2_comp_g, na.rm = TRUE)

# S_j: importance-weighted probability that each task keeps its class,
# read off the distance to the nearest threshold in noise SDs.
stab <- scores |>
  filter(!is.na(sub), !is.na(comp)) |>
  inner_join(select(task_df, onet_soc_code, task_id, importance),
             by = c("onet_soc_code", "task_id")) |>
  mutate(major = major_group(onet_soc_code)) |>
  left_join(select(sig_task, major, sigma2_sub_g, sigma2_comp_g), by = "major") |>
  mutate(sd_sub  = sqrt(coalesce(sigma2_sub_g,  ssub)),
         sd_comp = sqrt(coalesce(sigma2_comp_g, scomp)),
         z = pmin(abs(sub - thr["sub"]) / sd_sub, abs(comp - thr["comp"]) / sd_comp),
         p = pnorm(z)) |>
  group_by(onet_soc_code) |>
  summarise(S = sum(importance * p) / sum(importance), .groups = "drop") |>
  left_join(select(occ, onet_soc_code, E_j, classification), by = "onet_soc_code") |>
  mutate(uncertain = S < STAB$flag)

message(sum(stab$uncertain), " of ", nrow(stab), " occupations flagged (S < ", STAB$flag, ")")
save_csv(select(stab, onet_soc_code, E_j, classification, S, uncertain),
         file.path(PATHS$tables, "classification_stability.csv"))

# Flagged occupations should flip class across the noise variants more
# often than unflagged ones.
flip_f <- file.path(PATHS$cache, "calib_fliprate.csv")
if (file.exists(flip_f)) {
  val <- read_csv(flip_f, show_col_types = FALSE) |>
    inner_join(select(stab, onet_soc_code, uncertain), by = "onet_soc_code") |>
    group_by(uncertain) |>
    summarise(n = n(), flip_rate = mean(flipped), .groups = "drop")
  save_csv(val, file.path(PATHS$tables, "stability_validation.csv"))
}

# Invariance of the classified gap to the cutoff. Flags map up to the
# SOC3 groups the gap is built on; excluded groups drop out and shares
# renormalise within region.
map3 <- read_uk_onet_map() |>
  mutate(soc_uk = substr(gsub("[^0-9]", "", soc_uk4), 1, APS$soc_level)) |>
  select(onet_soc_code, soc_uk) |> distinct() |>
  mutate(onet_soc_code = as.character(onet_soc_code))
S_k <- stab |>
  mutate(onet_soc_code = as.character(onet_soc_code)) |>
  inner_join(map3, by = "onet_soc_code") |>
  group_by(soc_uk) |> summarise(S_k = mean(S), .groups = "drop")

panel <- read_csv(file.path(PATHS$cache, "region_panel.csv"), show_col_types = FALSE) |>
  mutate(soc_uk = as.character(soc_uk)) |>
  left_join(S_k, by = "soc_uk")

gap_at <- function(cut) {
  panel |>
    filter(is.na(S_k) | S_k >= cut) |>
    group_by(region) |>
    mutate(w = sigma / sum(sigma)) |>
    summarise(E_hat = sum(w * exposed), .groups = "drop") |>
    pivot_wider(names_from = region, values_from = E_hat) |>
    transmute(cutoff = cut, dE_hat = Scotland - rUK)
}
sweep <- map_dfr(c(0, STAB$grid), gap_at)   # cutoff 0 keeps every group
save_csv(sweep, file.path(PATHS$tables, "stability_cutoff_sweep.csv"))
message("dE_hat across cutoffs: ",
        paste(sweep$cutoff, sprintf("%.4f", sweep$dE_hat), sep = ":", collapse = " "))
