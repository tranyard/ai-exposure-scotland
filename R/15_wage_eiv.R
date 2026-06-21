# =====================================================================
# 15_wage_eiv.R  —  CONDITIONAL on ASHE access (Plan §1.5, §2.9).
# Worker-level wage regression with an exposure regressor measured with
# within-model error, plus the classical errors-in-variables correction
# for attenuation: beta^EIV = beta^OLS / (1 - lambda).
# Runs only if the ASHE microdata extract is present.
# =====================================================================
source(here::here("R", "00_config.R"))
suppressPackageStartupMessages(library(fixest))

ashe_f <- file.path(PATHS$aps, "ashe_microdata.csv")
if (!file.exists(ashe_f)) {
  message("ASHE microdata not present — wage module skipped (conditional path).")
} else {
uk       <- read_csv(file.path(PATHS$cache, "uk_soc3_scores.csv"), show_col_types = FALSE) |>
  mutate(soc_uk = as.character(soc_uk), E = E_uk / 100)
sigma2_g <- read_csv(file.path(PATHS$cache, "sigma2_g.csv"), show_col_types = FALSE)

# ASHE extract: ln_wage, soc_uk (3-digit), region, controls (age, sex, ft), year, industry.
ashe <- read_csv(ashe_f, show_col_types = FALSE) |>
  mutate(soc_uk = substr(gsub("[^0-9]","",soc_uk),1,3),
         Scot = as.integer(region == "Scotland")) |>
  inner_join(uk |> select(soc_uk, E), by = "soc_uk")

# --- OLS wage regression (eq. 10) ------------------------------------
m_ols <- feols(ln_wage ~ E * Scot + age + I(age^2) + sex + ft | industry + year,
               cluster = ~ soc_uk^region, data = ashe)

# --- Attenuation factor lambda = sigma_u^2 / Var(E) ------------------
# sigma_u^2 = mean group noise variance / 100^2 (E on the 0-1 scale).
sigma2_u <- mean(sigma2_g$sigma2_g, na.rm = TRUE) / 1e4
var_E    <- var(uk$E)
lambda   <- sigma2_u / var_E

b1_ols <- coef(m_ols)["E"]
b1_eiv <- b1_ols / (1 - lambda)
se_eiv <- se(m_ols)["E"] / (1 - lambda)   # leading-order delta-method SE

message(sprintf("lambda = %.4f | beta1 OLS = %.4f -> EIV = %.4f (SE %.4f)",
                lambda, b1_ols, b1_eiv, se_eiv))

eiv_tbl <- tibble(
  term = c("E (OLS)", "E (EIV-corrected)", "E x Scot (OLS)", "lambda", "sigma_u^2/Var(E)"),
  estimate = c(b1_ols, b1_eiv, coef(m_ols)["E:Scot"], lambda, sigma2_u/var_E))
print(eiv_tbl)
wage_path <- file.path(PATHS$tables, "wage_regression.txt")
wrote <- requireNamespace("modelsummary", quietly = TRUE) &&
  !inherits(try(modelsummary::modelsummary(
    list("OLS" = m_ols), output = wage_path,
    stars = c("*"=.1,"**"=.05,"***"=.01)), silent = TRUE), "try-error")
if (!wrote)
  writeLines(capture.output(print(etable(m_ols, se.below = TRUE, digits = 3))), wage_path)
save_csv(eiv_tbl, file.path(PATHS$tables, "wage_eiv.csv"))
}
