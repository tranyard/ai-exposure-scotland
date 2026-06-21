# =====================================================================
# tests/00_make_synthetic_data.R  —  Generate internally consistent
# stand-ins for every required input, so the whole pipeline can run
# offline (with MOCK_SCORING=1) before any real data or API spend.
# Writes into PATHS (redirected to a sandbox when SMOKE_ROOT is set).
# Only the LLM scores are NOT written here — those come from the mock
# scorer during the run, exercising the scoring scripts for real.
# =====================================================================
source(here::here("R", "00_config.R"))
suppressPackageStartupMessages(library(writexl))
set.seed(SEED)

# --- Occupations: spread across US-SOC first-digit groups 1-5 --------
# (first digit drives the O*NET-side calibration stratification) and
# across UK-SOC major groups 1-9 (drives sector/FE variety).
us_majors <- c(11, 13, 15, 17, 19, 21, 23, 25, 29, 31, 41, 43, 51, 53)
n_per <- 6
occ <- tibble(
  i = seq_len(length(us_majors) * n_per),
  us_major = rep(us_majors, each = n_per)
) |>
  mutate(
    onet_soc_code = sprintf("%02d-%04d.00", us_major, 1000 + i),
    us_soc        = substr(onet_soc_code, 1, 7),
    uk_major      = ((i - 1) %% 9) + 1,                       # 1..9
    uk_soc4       = sprintf("%d%03d", uk_major, 100 + i),
    uk_soc3       = substr(gsub("[^0-9]", "", uk_soc4), 1, 3),
    occupation_title = paste("Synthetic occupation", i),
    E_true        = pmin(pmax(round(rnorm(n(), 50, 18)), 5), 95)
  )

# --- Tasks: 5-10 per occupation, importance 1-5 ----------------------
tasks <- occ |>
  mutate(ntask = sample(5:10, n(), replace = TRUE)) |>
  uncount(ntask, .id = "t") |>
  mutate(task_id = sprintf("%s-T%02d", onet_soc_code, t),
         task = paste("Perform synthetic task", t, "for", occupation_title),
         task_type = if_else(t <= 3, "Core", "Supplemental"),
         importance = sample(1:5, n(), replace = TRUE))

# --- Write O*NET .xlsx (headers clean to the canonical names) --------
write_xlsx(
  tasks |> transmute(`O*NET-SOC Code` = onet_soc_code, `Task ID` = task_id,
                     Task = task, `Task Type` = task_type),
  file.path(PATHS$onet, "Task Statements.xlsx"))
write_xlsx(
  tasks |> transmute(`O*NET-SOC Code` = onet_soc_code, `Task ID` = task_id,
                     `Scale ID` = "IM", `Data Value` = importance,
                     `Recommend Suppress` = "N"),
  file.path(PATHS$onet, "Task Ratings.xlsx"))
write_xlsx(
  occ |> transmute(`O*NET-SOC Code` = onet_soc_code, Title = occupation_title),
  file.path(PATHS$onet, "Occupation Data.xlsx"))

# --- Crosswalk: direct UK SOC 2020 <-> O*NET-SOC (mirrors your sheet) -
# Same column layout as the "mapping SOC2020-ONET2019" file: a UK unit
# group column and an O*NET-SOC code column (order-independent on read).
save_csv(occ |> transmute(`SOC2020 Unit Group` = uk_soc4,
                          `SOC2020 Group Title` = paste("UK group", uk_major),
                          `O*NET-SOC Code` = onet_soc_code,
                          `Title` = occupation_title),
         file.path(PATHS$crosswalks, "soc2020_to_onet.csv"))

# --- APS employment: Scotland tilts toward UK majors 6 & 8 -----------
soc3 <- occ |> distinct(uk_soc3, uk_major)
regions <- c("Scotland", "England", "Wales", "Northern Ireland")
aps <- tidyr::crossing(soc3, region = regions, year = APS$years) |>
  mutate(
    base = exp(rnorm(n(), 8, 0.6)),
    scot_tilt = if_else(region == "Scotland" & uk_major %in% c(6, 8), 1.6,
                if_else(region == "Scotland" & uk_major %in% c(2, 3), 0.6, 1)),
    employment = round(base * scot_tilt)
  ) |>
  transmute(soc_uk = uk_soc3, region, year, employment)
save_csv(aps, file.path(PATHS$aps, "aps_employment_soc_region.csv"))

# --- ASHE microdata (conditional module) -----------------------------
soc_E <- occ |> group_by(uk_soc3) |> summarise(E = mean(E_true) / 100, .groups = "drop")
ashe <- tibble(id = 1:4000) |>
  mutate(
    soc_uk = sample(soc_E$uk_soc3, n(), replace = TRUE),
    region = sample(regions, n(), replace = TRUE, prob = c(.09, .77, .08, .06)),
    Scot   = as.integer(region == "Scotland"),
    age = round(runif(n(), 18, 64)), sex = rbinom(n(), 1, .5), ft = rbinom(n(), 1, .75),
    industry = sample(LETTERS[1:8], n(), replace = TRUE),
    year = sample(APS$years, n(), replace = TRUE)
  ) |>
  left_join(soc_E, by = c("soc_uk" = "uk_soc3")) |>
  mutate(ln_wage = 2.4 + 0.5 * E + 0.04 * Scot + 0.02 * age - 0.0002 * age^2 +
                   0.08 * ft + 0.05 * sex + rnorm(n(), 0, 0.25)) |>
  select(ln_wage, soc_uk, region, age, sex, ft, industry, year)
save_csv(ashe, file.path(PATHS$aps, "ashe_microdata.csv"))

# --- SFC / OBR calibration inputs ------------------------------------
save_csv(tibble(scenario = c("lo", "central", "hi"), kappa = c(0.10, 0.18, 0.27)),
         file.path(PATHS$sfc, "acemoglu_calibration.csv"))
save_csv(occ |> distinct(soc_uk = uk_soc3) |> mutate(pi = runif(n(), 0.5, 1.5)),
         file.path(PATHS$sfc, "task_cost_share.csv"))
save_csv(tibble(region = c("Scotland", "rUK"),
                gva_base = c(170e9, 2100e9), labour_share = c(0.58, 0.58)),
         file.path(PATHS$sfc, "gdp_passthrough.csv"))
save_csv(occ |> transmute(onet_soc_code,
                          log_volume = round(rnorm(n(), 6, 1.5), 2),
                          contested = rbinom(n(), 1, 0.3)),
         file.path(PATHS$sfc, "info_env.csv"))

message("\nSynthetic data written to: ", ROOT,
        "\noccupations=", nrow(occ), " tasks=", nrow(tasks),
        " UK SOC3 groups=", n_distinct(occ$uk_soc3))
