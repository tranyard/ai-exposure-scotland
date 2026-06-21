# =====================================================================
# 09_employment_regressions.R  —  Employment regressions (Plan §1.3).
# Occupation level (eq. 4): y_{k,r} on E_bar_k * Scot, employment-weighted,
#   sector FE, clustered by occupation. y = log employment and share.
# First-difference secondary: dlog employment on E_bar_k * Scot.
# Industry level: the sector analogue, descriptive.
# =====================================================================
source(here::here("R", "00_config.R"))
suppressPackageStartupMessages(library(fixest))

panel <- read_csv(file.path(PATHS$cache, "region_panel.csv"), show_col_types = FALSE) |>
  mutate(Scot = as.integer(region == "Scotland"),
         major = major_group(soc_uk),
         E_bar = E_uk / 100,
         log_emp = log(pmax(l_pool, 1)))

# --- Occupation-level (eq. 4): levels --------------------------------
m_logemp <- feols(log_emp ~ E_bar * Scot | major, weights = ~ l_pool,
                  cluster = ~ soc_uk, data = panel)
m_share  <- feols(sigma   ~ E_bar * Scot | major, weights = ~ l_pool,
                  cluster = ~ soc_uk, data = panel)

# --- First-difference (employment dynamics, secondary) ---------------
yr <- read_csv(file.path(PATHS$cache, "aps_year.csv"), show_col_types = FALSE) |>
  inner_join(distinct(panel, soc_uk, E_bar, major), by = "soc_uk") |>
  mutate(Scot = as.integer(region == "Scotland")) |>
  arrange(soc_uk, region, year) |>
  group_by(soc_uk, region) |>
  mutate(dlog_emp = log(pmax(employment, 1)) - lag(log(pmax(employment, 1)))) |>
  ungroup() |> filter(!is.na(dlog_emp))
m_fd <- feols(dlog_emp ~ E_bar * Scot | major + year, weights = ~ employment,
              cluster = ~ soc_uk, data = yr)

# --- Industry level (sector x region), descriptive -------------------
sector <- read_csv(file.path(PATHS$tables, "sector_exposure.csv"), show_col_types = FALSE) |>
  mutate(Scot = as.integer(region == "Scotland"))
m_ind <- feols(E_rs ~ Scot, weights = ~ emp, data = sector)   # mean gap across sectors

models <- list("log emp" = m_logemp, "emp share" = m_share,
               "Δlog emp" = m_fd, "sector rate" = m_ind)
tbl_path <- file.path(PATHS$tables, "employment_regressions.txt")
wrote <- requireNamespace("modelsummary", quietly = TRUE) &&
  !inherits(try(modelsummary::modelsummary(
    models, output = tbl_path, stars = c("*"=.1,"**"=.05,"***"=.01),
    gof_omit = "AIC|BIC|RMSE|Within|IC"), silent = TRUE), "try-error")
if (!wrote)
  writeLines(capture.output(print(
    etable(m_logemp, m_share, m_fd, m_ind, se.below = TRUE, digits = 3,
           fitstat = ~ n + r2))), tbl_path)
message("zeta (E_bar x Scot), log emp: ",
        signif(coef(m_logemp)["E_bar:Scot"], 3))
saveRDS(models, file.path(PATHS$cache, "employment_models.rds"))
