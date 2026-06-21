# =====================================================================
# 06_crosswalk.R  —  O*NET-SOC 2019 -> UK SOC 2020 (Plan §1.1).
# DIRECT single-step mapping using the SOC2020<->O*NET-SOC concordance
# (your "mapping SOC2020-ONET2019" sheet). O*NET 30.3 uses the O*NET-SOC
# 2019 taxonomy, so the 2019 concordance matches the task data. The US
# SOC 2018 crosswalk is NOT needed on this path.
#
# Aggregates O*NET occupation exposure to UK SOC 2020 unit groups
# (4-digit), then to the APS working level (3-digit minor groups).
# Many-to-many is handled correctly: a UK group averages over all the
# O*NET occupations mapped to it; an O*NET occupation may feed several
# UK groups.
# =====================================================================
source(here::here("R", "00_config.R"))

occ <- read_csv(file.path(PATHS$cache, "occupation_scores_V0.csv"), show_col_types = FALSE) |>
  mutate(onet_soc_code = as.character(onet_soc_code))
map <- read_uk_onet_map()

# Share-based classification rule (identical to 05's occupation rule).
classify_shares <- function(Sub, Comp) {
  Exp <- Sub + Comp
  case_when(Exp <= 1e-9       ~ "Unexposed",
            Sub > 0.5 * Exp   ~ "Substituted",
            TRUE              ~ "Complemented")
}

# Optional concordance weights; equal weight if the column is absent.
if (!"weight" %in% names(map)) map$weight <- 1

# --- Aggregate to UK SOC 2020 unit group (4-digit) -------------------
uk4 <- map |>
  inner_join(occ, by = "onet_soc_code") |>
  group_by(soc_uk4) |>
  summarise(n_onet   = n(),
            E_uk     = weighted.mean(E_j,      weight),
            E_uk_sub = weighted.mean(E_sub_j,  weight),
            E_uk_comp= weighted.mean(E_comp_j, weight),
            Sub_uk   = weighted.mean(Sub_j,    weight),
            Comp_uk  = weighted.mean(Comp_j,   weight),
            .groups = "drop") |>
  mutate(classification = classify_shares(Sub_uk, Comp_uk))

# Diagnostics: coverage in both directions.
n_unmapped_onet <- occ |> anti_join(map, by = "onet_soc_code") |> nrow()
message("O*NET occupations not mapped to any UK group: ", n_unmapped_onet,
        " of ", nrow(occ))

# --- Aggregate 4-digit -> APS working level (3-digit minor) ----------
uk3 <- uk4 |>
  mutate(soc_uk = substr(gsub("[^0-9]", "", soc_uk4), 1, APS$soc_level)) |>
  group_by(soc_uk) |>
  summarise(E_uk     = mean(E_uk),     E_uk_sub = mean(E_uk_sub),
            E_uk_comp= mean(E_uk_comp), Sub_uk   = mean(Sub_uk),
            Comp_uk  = mean(Comp_uk),   .groups = "drop") |>
  mutate(classification = classify_shares(Sub_uk, Comp_uk))

message("UK SOC 2020 unit groups (4-digit): ", nrow(uk4),
        " | minor groups (", APS$soc_level, "-digit): ", nrow(uk3))
save_csv(uk4, file.path(PATHS$cache, "uk_soc4_scores.csv"))
save_csv(uk3, file.path(PATHS$cache, "uk_soc3_scores.csv"))
