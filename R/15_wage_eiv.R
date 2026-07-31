# Worker-level wage regression and EIV correction.
#
# Dependent variable is log gross hourly pay (set upstream in 07 as
# log(V$hourpay)). Reported columns: (1) baseline, (2) + education and
# public-sector controls, (3) + region FE, (4) + London slope. The
# occupation x region clustering fit and the ex-London fit are estimated
# for in-text comparison but not tabled.
#
# Clustering is by occupation (soc_uk): E varies only at soc_uk and the
# identical score sits on both sides of the border, so occ x region
# clusters would treat it as two independent regressors and assume away
# the cross-region error covariance the interaction nets out.
source(here::here("R", "00_config.R"))
suppressPackageStartupMessages(library(fixest))

ashe_f <- file.path(PATHS$aps, "ashe_microdata.csv")
aps_f  <- file.path(PATHS$cache, "aps_worker_wages.csv")

src <- if (file.exists(ashe_f)) "ashe" else if (file.exists(aps_f)) "aps" else NA
if (is.na(src)) {
  message("No wage data (ASHE or APS) present - wage module skipped.")
} else {
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
    dat <- read_csv(aps_f, show_col_types = FALSE) |>
      mutate(soc_uk = as.character(soc_uk), wgt = piwt,
             sex = factor(sex), ft = factor(ft), industry = factor(industry))
  }

  dat <- dat |>
    mutate(Scot = as.integer(region == "Scotland")) |>
    inner_join(uk |> select(soc_uk, E), by = "soc_uk")

  if ("gor" %in% names(dat))
    dat <- dat |> mutate(gor = dplyr::na_if(trimws(as.character(gor)), ""))

  pick <- function(df, ...) {
    cand <- c(...); hit <- cand[cand %in% names(df)]
    if (length(hit)) hit[[1]] else NA_character_
  }

  # 07 emits columns named `education` and `public`; the aliases cover a
  # hand-built extract that used the raw APS names.
  edu_col <- pick(dat, "education", "hiqual", "hiqul22d", "hiqul15d",
                  "highest_qual", "degree")
  pub_col <- pick(dat, "public", "publicr", "pubsect", "sector")
  has_edu <- !is.na(edu_col)
  has_pub <- !is.na(pub_col)
  if (has_edu) dat <- dat |> mutate(education = droplevels(factor(.data[[edu_col]])))
  if (has_pub) dat <- dat |> mutate(public    = droplevels(factor(.data[[pub_col]])))
  add_terms  <- c(if (has_edu) "education", if (has_pub) "public")
  have_added <- length(add_terms) > 0
  if (!have_added)
    message("Neither education nor a public-sector field on the extract; ",
            "columns (2)-(4) carry no added controls.")

  multi_year <- dplyr::n_distinct(dat$year) > 1
  fe <- if (multi_year) "industry + year" else "industry"
  Xb <- "age + I(age^2) + sex + ft"
  Xa <- paste(c(Xb, add_terms), collapse = " + ")

  fml1 <- as.formula(paste("ln_wage ~ E * Scot +", Xb, "|", fe))
  m1     <- feols(fml1, weights = ~ wgt, cluster = ~ soc_uk,        data = dat)
  m1_xr  <- feols(fml1, weights = ~ wgt, cluster = ~ soc_uk^region, data = dat)

  # Attenuation factor lambda = sigma_u^2 / Var(E).
  sigma2_u <- mean(sigma2_g$sigma2_g, na.rm = TRUE) / 1e4
  var_E    <- var(uk$E)
  lambda   <- sigma2_u / var_E

  b1_ols <- coef(m1)["E"]
  b1_eiv <- b1_ols / (1 - lambda)
  se_eiv <- se(m1)["E"] / (1 - lambda)   # leading-order delta-method SE

  # The closed form b/(1 - lambda) is a single-regressor result. beta3 is
  # read as the gap between the rUK slope (beta1) and the Scottish slope
  # (beta1 + beta3), each one mismeasured regressor within its own
  # region. sigma_u^2 is a property of the shared occupation score and
  # Var(E) differs across regions only through employment composition, so
  # the two reliabilities are approximately equal and the common factor
  # 1/(1 - lambda) carries beta3 as well.
  int_ols    <- if ("E:Scot" %in% names(coef(m1))) coef(m1)["E:Scot"] else NA_real_
  int_eiv    <- int_ols / (1 - lambda)
  se_int_eiv <- if (!is.na(int_ols)) se(m1)["E:Scot"] / (1 - lambda) else NA_real_

  message(sprintf(
    "[%s] lambda = %.4f | beta1 %.4f -> %.4f (SE %.4f) | beta3 %.4f -> %.4f (SE %.4f)",
    toupper(src), lambda, b1_ols, b1_eiv, se_eiv, int_ols, int_eiv, se_int_eiv))

  eiv_tbl <- tibble(
    term = c("E (OLS)", "E (EIV-corrected)",
             "E x Scot (OLS)", "E x Scot (EIV-corrected)",
             "lambda", "sigma_u^2/Var(E)"),
    estimate = c(b1_ols, b1_eiv, int_ols, int_eiv, lambda, sigma2_u / var_E))
  save_csv(eiv_tbl, file.path(PATHS$tables, "wage_eiv.csv"))

  has_gor_w <- src == "aps" && "gor" %in% names(dat) && any(!is.na(dat$gor))
  m2 <- m3 <- m4 <- m_exldn <- NULL
  if (!has_gor_w) {
    message("gor not on the wage extract - region-FE/London specs skipped.")
  } else {
    # A wave carrying no usable region would let the naive
    # "Scotland | gor observed" rule keep that wave's Scots while dropping
    # all its rUK workers, unbalancing the London contrast within-year.
    # Restrict the region/London specs to waves whose rUK side carries
    # region; the baseline gradient and the EIV keep all waves.
    gor_cov <- dat |> filter(region == "rUK") |>
      group_by(year) |>
      summarise(cov = mean(!is.na(gor)), .groups = "drop")
    gor_years <- sort(gor_cov$year[gor_cov$cov >= 0.5])
    dropped_years <- setdiff(sort(unique(dat$year)), gor_years)
    if (length(dropped_years))
      message("Region/London specs exclude wave(s) with no usable rUK region: ",
              paste(dropped_years, collapse = ", "))

    dat_g <- dat |>
      filter(year %in% gor_years, region == "Scotland" | !is.na(gor)) |>
      mutate(london  = as.integer(region != "Scotland" &
                                    gor == APS$codes$london_gor),
             gor_lab = if_else(region == "Scotland", "Scotland", gor))

    w_drop <- 1 - sum(dat_g$wgt[dat_g$region == "rUK"]) /
      sum(dat$wgt[dat$region == "rUK"])
    if (length(dropped_years) == 0 && w_drop > 0.05)
      message(sprintf("missing region exceeds 5%% of rUK weight (%.1f%%) within the retained waves.",
                      100 * w_drop))

    # The Scot main effect is one of the gor_lab FE levels in (3), so it
    # is omitted there rather than estimated.
    fml2 <- as.formula(paste("ln_wage ~ E * Scot +", Xa, "|", fe))
    fml3 <- as.formula(paste("ln_wage ~ E + E:Scot +", Xa, "|", fe, "+ gor_lab"))
    fml4 <- as.formula(paste("ln_wage ~ E * Scot + E:london + london +", Xa, "|", fe))

    m2      <- feols(fml2, weights = ~ wgt, cluster = ~ soc_uk, data = dat_g)
    m3      <- feols(fml3, weights = ~ wgt, cluster = ~ soc_uk, data = dat_g)
    m4      <- feols(fml4, weights = ~ wgt, cluster = ~ soc_uk, data = dat_g)
    m_exldn <- feols(fml2, weights = ~ wgt, cluster = ~ soc_uk,
                     data = filter(dat_g, london == 0))

    b3 <- \(m) if (!is.null(m) && "E:Scot" %in% names(coef(m)))
      sprintf("%+.4f (%.4f)", coef(m)["E:Scot"], se(m)["E:Scot"]) else "-"
    message("E x Scot | (1) ", b3(m1),
            " | occ x region cl. ", b3(m1_xr),
            " | (2) ", b3(m2),
            " | (3) ", b3(m3),
            " | (4) ", b3(m4),
            " | ex-London ", b3(m_exldn),
            if (!is.null(m4) && "E:london" %in% names(coef(m4)))
              sprintf(" [E x London %+.4f (%.4f)]",
                      coef(m4)["E:london"], se(m4)["E:london"]) else "")
  }

  cf <- function(m, nm, f = coef) { if (is.null(m)) return(NA_real_)
    v <- f(m); if (nm %in% names(v)) v[[nm]] else NA_real_ }
  reg_tbl <- imap_dfr(
    purrr::compact(list("(1) baseline"       = m1,
                        "(2) +controls"      = m2,
                        "(3) +region FE"     = m3,
                        "(4) +London slope"  = m4)),
    \(m, nm) tibble(spec = nm, n = nobs(m),
                    b_E = cf(m, "E"),          se_E = cf(m, "E", se),
                    b_EScot = cf(m, "E:Scot"), se_EScot = cf(m, "E:Scot", se),
                    b_ELondon = cf(m, "E:london"), se_ELondon = cf(m, "E:london", se)))
  save_csv(reg_tbl, file.path(PATHS$tables, "wage_specs_key.csv"))

  # booktabs + threeparttable, no siunitx. SEs in parentheses beneath each
  # estimate; an "Added controls" panel row carries superscript b.
  emit_wage_table <- function(cols, path) {
    star <- function(p) if (is.na(p)) "" else
      if (p < .01) "^{***}" else if (p < .05) "^{**}" else if (p < .1) "^{*}" else ""
    est_cell <- function(m, nm) {
      if (is.null(m) || !nm %in% names(coef(m))) return("")
      sprintf("$%.3f%s$", coef(m)[[nm]], star(fixest::pvalue(m)[[nm]]))
    }
    se_cell <- function(m, nm) {
      if (is.null(m) || !nm %in% names(coef(m))) return("")
      sprintf("$(%.3f)$", se(m)[[nm]])
    }
    within_r2 <- function(m) {
      v <- tryCatch(unname(fixest::r2(m, "wr2")), error = function(e) NA_real_)
      if (is.na(v)) "" else sprintf("%.3f", v)
    }
    yn <- function(x) if (isTRUE(x)) "Yes" else ""
    ncol   <- length(cols)
    colspec <- paste0("l ", paste(rep("c", ncol), collapse = ""))
    hd_num  <- paste(sprintf("(%d)", seq_len(ncol)), collapse = " & ")
    hd_tag1 <- paste(vapply(cols, function(c) c$h1, ""), collapse = " & ")
    hd_tag2 <- paste(vapply(cols, function(c) c$h2, ""), collapse = " & ")

    coef_row <- function(label, nm) {
      e <- paste(vapply(cols, function(c) est_cell(c$m, nm), ""), collapse = " & ")
      s <- paste(vapply(cols, function(c) se_cell(c$m, nm),  ""), collapse = " & ")
      c(paste0(label, " & ", e, " \\\\"),
        paste0(" & ", s, " \\\\"),
        "\\addlinespace[2pt]")
    }
    panel_row <- function(label, vals)
      paste0(label, " & ", paste(vals, collapse = " & "), " \\\\")

    L <- c(
      "\\begin{threeparttable}",
      "\\small",
      "\\setlength{\\tabcolsep}{5pt}",
      "\\renewcommand{\\arraystretch}{1.05}",
      paste0("\\begin{tabular}{", colspec, "}"),
      "\\toprule",
      paste0(" & ", hd_num,  " \\\\"),
      paste0(" & ", hd_tag1, " \\\\"),
      paste0(" & ", hd_tag2, " \\\\"),
      "\\midrule",
      coef_row("Exposure $E$",                "E"),
      coef_row("Exposure $\\times$ Scotland", "E:Scot"),
      coef_row("Scotland",                    "Scot"),
      coef_row("Exposure $\\times$ London",   "E:london"),
      coef_row("London",                      "london"),
      "\\midrule",
      panel_row("Demographic controls\\tnote{a}",
                vapply(cols, function(c) yn(TRUE), "")),
      panel_row("Added controls\\tnote{b}",
                vapply(cols, function(c) yn(c$added), "")),
      panel_row("Industry FE", vapply(cols, function(c) yn(TRUE), "")),
      panel_row("Year FE",     vapply(cols, function(c) yn(TRUE), "")),
      panel_row("Region (GOR) FE", vapply(cols, function(c) yn(c$regfe), "")),
      panel_row("Clustering",  vapply(cols, function(c) c$cl, "")),
      panel_row("Sample",      vapply(cols, function(c) c$sample, "")),
      "\\addlinespace[2pt]",
      panel_row("Observations",
                vapply(cols, function(c) formatC(nobs(c$m), big.mark = ",", format = "d"), "")),
      panel_row("Within $R^2$", vapply(cols, function(c) within_r2(c$m), "")),
      "\\bottomrule",
      "\\end{tabular}",
      "\\begin{tablenotes}[flushleft]",
      "\\footnotesize",
      "\\item Dependent variable: log gross hourly pay. Worker-level regressions",
      "of \\cref{eq:wage} on the pooled APS 2022--2025, weighted by the APS income",
      "weight. Standard errors, clustered by occupation (the level at which the",
      "exposure score varies), in parentheses. Column (1) is the full-sample",
      "baseline; columns (2)--(4) add the controls of note b and restrict to",
      "observations with an observed regional code, so column (2) is the",
      "like-for-like comparator for the London treatments in (3)--(4). The",
      "Scotland main effect is absorbed by the regional fixed effects in column",
      "(3) and is therefore not separately estimable.",
      "\\item[a] Age, age$^2$, sex, and full-time/part-time status.",
      "\\item[b] Highest qualification (education) and a public-sector indicator.",
      "\\item[] $^{*}\\,p<0.1$, $^{**}\\,p<0.05$, $^{***}\\,p<0.01$.",
      "\\end{tablenotes}",
      "\\end{threeparttable}")
    writeLines(L, path)
  }

  wage_path <- file.path(PATHS$tables, "wage_specs_key.tex")
  cols <- purrr::compact(list(
    if (!is.null(m1)) list(m = m1, added = FALSE, regfe = FALSE, sample = "Full",
                           cl = "Occ.", h1 = "Baseline",   h2 = "(full)"),
    if (!is.null(m2)) list(m = m2, added = have_added, regfe = FALSE, sample = "GOR",
                           cl = "Occ.", h1 = "$+$Controls", h2 = ""),
    if (!is.null(m3)) list(m = m3, added = have_added, regfe = TRUE,  sample = "GOR",
                           cl = "Occ.", h1 = "$+$Region",   h2 = "FE"),
    if (!is.null(m4)) list(m = m4, added = have_added, regfe = FALSE, sample = "GOR",
                           cl = "Occ.", h1 = "$+$London",   h2 = "slope")))
  emit_wage_table(cols, wage_path)
}
