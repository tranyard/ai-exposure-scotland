# =====================================================================
# 15_wage_eiv.R  —  Worker-level wage regression + EIV correction
# =====================================================================
source(here::here("R", "00_config.R"))
suppressPackageStartupMessages(library(fixest))

ashe_f <- file.path(PATHS$aps, "ashe_microdata.csv")
aps_f  <- file.path(PATHS$cache, "aps_worker_wages.csv")

src <- if (file.exists(ashe_f)) "ashe" else if (file.exists(aps_f)) "aps" else NA
if (is.na(src)) {
  message("No wage data (ASHE or APS) present — wage module skipped.")
} else {
  message("Wage module source: ", toupper(src))

  uk       <- read_csv(file.path(PATHS$cache, "uk_soc3_scores.csv"),
                       show_col_types = FALSE) |>
    mutate(soc_uk = as.character(soc_uk), E = E_uk / 100)
  sigma2_g <- read_csv(file.path(PATHS$cache, "sigma2_g.csv"),
                       show_col_types = FALSE)

  if (src == "ashe") {
    dat <- read_csv(ashe_f, show_col_types = FALSE) |>
      mutate(soc_uk = substr(gsub("[^0-9]", "", soc_uk), 1, APS$soc_level),
             wgt = 1)
  } else {
    # aps_worker_wages.csv carries ln_wage, soc_uk (3-digit), region,
    # age, sex, ft, industry, year, and the income weight (piwt
    dat <- read_csv(aps_f, show_col_types = FALSE) |>
      mutate(soc_uk = as.character(soc_uk), wgt = piwt,
             sex = factor(sex), ft = factor(ft), industry = factor(industry))
  }

  dat <- dat |>
    mutate(Scot = as.integer(region == "Scotland")) |>
    inner_join(uk |> select(soc_uk, E), by = "soc_uk")

  # --- OLS wage regression
  # Drop the year FE when a single wave is present; keep industry FE.
  multi_year <- dplyr::n_distinct(dat$year) > 1
  fe  <- if (multi_year) "industry + year" else "industry"
  fml <- as.formula(paste("ln_wage ~ E * Scot + age + I(age^2) + sex + ft |", fe))

  m_ols <- feols(fml, weights = ~ wgt, cluster = ~ soc_uk^region, data = dat)

  # --- Attenuation factor lambda = sigma_u^2 / Var(E)
  # sigma_u^2 = mean group noise variance / 100^2
  sigma2_u <- mean(sigma2_g$sigma2_g, na.rm = TRUE) / 1e4
  var_E    <- var(uk$E)
  lambda   <- sigma2_u / var_E

  b1_ols <- coef(m_ols)["E"]
  b1_eiv <- b1_ols / (1 - lambda)
  se_eiv <- se(m_ols)["E"] / (1 - lambda)   # leading-order delta-method SE

  message(sprintf("[%s] lambda = %.4f | beta1 OLS = %.4f -> EIV = %.4f (SE %.4f)",
                  toupper(src), lambda, b1_ols, b1_eiv, se_eiv))

  int <- if ("E:Scot" %in% names(coef(m_ols))) coef(m_ols)["E:Scot"] else NA_real_
  eiv_tbl <- tibble(
    term = c("E (OLS)", "E (EIV-corrected)", "E x Scot (OLS)",
             "lambda", "sigma_u^2/Var(E)"),
    estimate = c(b1_ols, b1_eiv, int, lambda, sigma2_u / var_E))
  print(eiv_tbl)

  wage_path <- file.path(PATHS$tables, "wage_regression.txt")
  wrote <- requireNamespace("modelsummary", quietly = TRUE) &&
    !inherits(try(modelsummary::modelsummary(
      list("OLS" = m_ols), output = wage_path,
      stars = c("*" = .1, "**" = .05, "***" = .01)), silent = TRUE), "try-error")
  if (!wrote)
    writeLines(capture.output(print(etable(m_ols, se.below = TRUE, digits = 3))),
               wage_path)
  save_csv(eiv_tbl, file.path(PATHS$tables, "wage_eiv.csv"))
}
