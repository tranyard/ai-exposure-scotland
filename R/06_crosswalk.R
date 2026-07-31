# O*NET occupation scores -> UK SOC 2020 unit groups (SOC4) and minor
# groups (SOC3). This is the definition uk3_continuous() in 00_config.R
# mirrors.
source(here::here("R", "00_config.R"))

map <- read_uk_onet_map()
if (!"weight" %in% names(map)) map$weight <- 1

crosswalk_one <- function(set) {
  suffix <- if (set == "central") "" else paste0("_", set)
  occ <- read_csv(file.path(PATHS$cache, paste0("occupation_scores_V0", suffix, ".csv")),
                  show_col_types = FALSE) |>
    mutate(onet_soc_code = as.character(onet_soc_code))

  uk4 <- map |>
    inner_join(occ, by = "onet_soc_code") |>
    group_by(soc_uk4) |>
    summarise(across(c(E_j, E_sub_j, E_comp_j, E_sat_j, Sub_j, Comp_j,
                       PM_sub_j, PM_comp_j),
                     \(x) weighted.mean(x, weight)), .groups = "drop") |>
    rename(E_uk = E_j, E_uk_sub = E_sub_j, E_uk_comp = E_comp_j, E_uk_sat = E_sat_j,
           Sub_uk = Sub_j, Comp_uk = Comp_j,
           PM_sub_uk = PM_sub_j, PM_comp_uk = PM_comp_j) |>
    mutate(classification    = classify_occ(Sub_uk, Comp_uk),
           classification_pm = classify_pm(PM_sub_uk, PM_comp_uk))

  uk3 <- uk4 |>
    mutate(soc_uk = substr(gsub("[^0-9]", "", soc_uk4), 1, APS$soc_level)) |>
    group_by(soc_uk) |>
    summarise(across(c(E_uk, E_uk_sub, E_uk_comp, E_uk_sat, Sub_uk, Comp_uk,
                       PM_sub_uk, PM_comp_uk), mean),
              .groups = "drop") |>
    mutate(classification    = classify_occ(Sub_uk, Comp_uk),
           classification_pm = classify_pm(PM_sub_uk, PM_comp_uk))

  save_csv(uk4, file.path(PATHS$cache, paste0("uk_soc4_scores", suffix, ".csv")))
  save_csv(uk3, file.path(PATHS$cache, paste0("uk_soc3_scores", suffix, ".csv")))
}

invisible(lapply(names(THRESHOLDS), crosswalk_one))
