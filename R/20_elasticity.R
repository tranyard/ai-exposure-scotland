source(here::here("R", "00_config.R"))

SCHEDULE <- list(
  `2026-27` = list(
    pa = 12570, taper_start = 100000, taper_rate = 0.5,
    bands = tibble::tribble(
      ~from,   ~rate,
      12570,  0.19,   # starter
      15397,  0.20,   # basic
      27491,  0.21,   # intermediate
      43662,  0.42,   # higher
      75000,  0.45,   # advanced
      125140,  0.48    # top
    )
  )
)
TAX_YEAR <- "2026-27"

ANCHOR <- list(
  sit_outturn_m   = 18635,   # HMRC Scottish Income Tax outturn, 2024-25
  sit_forecast_m  = 21508,   # SFC forecast NSND revenue, 2026-27
  net_position_m  = 969,     # SFC Jan 2026 forecast net position, 2026-27
  base_for_fiscal = 18600,   # base used in tab:fiscal (£m)
  capital_share   = 1/3      # TFP -> labour productivity: 1/(1 - alpha)
)

WEEKS <- 52
FALLBACK_HOURS <- c(`1` = 37, `2` = 18)   # FTPT: 1 full time, 2 part time
HOURS_ALIASES  <- c("hours", "ttushr", "tothrs", "usual_hours", "hrs")

# The personal allowance taper is carried explicitly: over the taper range
# each extra pound of income exposes a further half pound, so the
# effective marginal rate is 1.5x the band rate.
allowance <- function(y, s) {
  pmax(0, s$pa - pmax(0, y - s$taper_start) * s$taper_rate)
}

liability <- function(y, s) {
  ti <- pmax(0, y - allowance(y, s))
  edges <- s$bands$from - s$pa
  edges[1] <- 0
  upper <- c(edges[-1], Inf)
  out <- numeric(length(ti))
  for (b in seq_len(nrow(s$bands)))
    out <- out + pmax(0, pmin(ti, upper[b]) - edges[b]) * s$bands$rate[b]
  out
}

marginal_rate <- function(y, s) {
  ti   <- pmax(0, y - allowance(y, s))
  edges <- s$bands$from - s$pa; edges[1] <- 0
  idx  <- findInterval(ti, edges, left.open = TRUE)
  m    <- dplyr::if_else(ti <= 0, 0, s$bands$rate[pmax(1, idx)])
  in_taper <- y > s$taper_start & y <= s$taper_start + s$pa / s$taper_rate
  dplyr::if_else(in_taper, m * (1 + s$taper_rate), m)
}

aps_f <- require_file(file.path(PATHS$cache, "aps_worker_wages.csv"),
                      "Run 07_aps_employment.R first.")
dat <- readr::read_csv(aps_f, show_col_types = FALSE) |>
  dplyr::mutate(soc_uk = as.character(soc_uk))

if (!"hourpay" %in% names(dat)) {
  if (!"ln_wage" %in% names(dat))
    stop("Neither hourpay nor ln_wage on the wage extract.", call. = FALSE)
  dat$hourpay <- exp(dat$ln_wage)
}


hrs_col <- HOURS_ALIASES[HOURS_ALIASES %in% names(dat)][1]
if (is.na(hrs_col)) {
  message("no weekly-hours column on the wage extract; imputing ",
          FALLBACK_HOURS[["1"]], "h full-time / ", FALLBACK_HOURS[["2"]], "h part-time.")
  dat$hours <- unname(FALLBACK_HOURS[as.character(dat$ft)])
  hours_source <- sprintf("imputed (%gh FT / %gh PT)",
                          FALLBACK_HOURS[["1"]], FALLBACK_HOURS[["2"]])
} else {
  dat$hours <- aps_num(dat[[hrs_col]])
  hours_source <- paste0("APS ", hrs_col)
}

scot <- dat |>
  dplyr::filter(region == "Scotland",
                is.finite(hourpay), hourpay > 0,
                is.finite(hours),   hours   > 0,
                is.finite(piwt),    piwt    > 0) |>
  dplyr::mutate(y = hourpay * hours * WEEKS)

if (nrow(scot) == 0) stop("No usable Scottish wage observations.", call. = FALSE)

s <- SCHEDULE[[TAX_YEAR]]
scot <- scot |>
  dplyr::mutate(T_i = liability(y, s), m_i = marginal_rate(y, s))

W <- sum(scot$piwt)
wm <- function(x) sum(scot$piwt * x) / W

num  <- wm(scot$m_i * scot$y)
den  <- wm(scot$T_i)
eps  <- num / den
mtr_wtd  <- num / wm(scot$y)
avg_rate <- den / wm(scot$y)

uk <- tryCatch(
  readr::read_csv(file.path(PATHS$cache, "uk_soc3_scores.csv"),
                  show_col_types = FALSE) |>
    dplyr::mutate(soc_uk = as.character(soc_uk), E = E_uk / 100) |>
    dplyr::select(soc_uk, E),
  error = function(e) NULL)

eps_E <- NA_real_
if (!is.null(uk)) {
  sx <- dplyr::inner_join(scot, uk, by = "soc_uk")
  if (nrow(sx) > 0) {
    Wx  <- sum(sx$piwt); wmx <- function(x) sum(sx$piwt * x) / Wx
    Ebar_y <- wmx(sx$y * sx$E) / wmx(sx$y)
    eps_E  <- wmx(sx$m_i * sx$y * sx$E) / (Ebar_y * wmx(sx$T_i))
  }
}


implied_m <- sum(scot$piwt * scot$T_i) / 1e6
coverage  <- implied_m / ANCHOR$sit_outturn_m

message(sprintf(
  "NSND elasticity (%s schedule, hours: %s): eps = %.3f | eps_E = %s | coverage %.2f",
  TAX_YEAR, hours_source, eps,
  if (is.na(eps_E)) "n/a" else sprintf("%.3f", eps_E), coverage))

els_tbl <- tibble::tibble(
  Quantity = c(
    "Income-weighted average marginal rate",
    "Average rate on total income",
    "Elasticity of NSND revenue to earnings, eps",
    "Incidence-weighted elasticity, eps_E",
    "Implied aggregate liability (GBP m)",
    "HMRC outturn, 2024-25 (GBP m)",
    "Coverage ratio (implied / outturn)",
    "Scottish observations",
    "Tax year of schedule"),
  Value = c(
    sprintf("%.3f", mtr_wtd),
    sprintf("%.3f", avg_rate),
    sprintf("%.3f", eps),
    if (is.na(eps_E)) "n/a" else sprintf("%.3f", eps_E),
    sprintf("%.0f", implied_m),
    sprintf("%.0f", ANCHOR$sit_outturn_m),
    sprintf("%.2f", coverage),
    format(nrow(scot), big.mark = ","),
    TAX_YEAR))
save_csv(els_tbl, file.path(PATHS$tables, "nsnd_elasticity.csv"))

# Fiscal translation: TFP -> labour productivity -> earnings -> revenue -> net position.
amp <- 1 / (1 - ANCHOR$capital_share)

dtfp <- tryCatch({
  readr::read_csv(table_csv("acemoglu_differential"), show_col_types = FALSE) |>
    dplyr::filter(channel == "composite") |>
    dplyr::select(scenario, dTFP = diff)
}, error = function(e)
  tibble::tibble(scenario = c("lo", "central", "hi"),
                 dTFP = c(-0.02211, -0.02895, -0.03659)))

fiscal <- dtfp |>
  dplyr::mutate(
    dLP        = dTFP * amp,                                  # pp cumulative
    d_earn_pa  = dLP / 10,                                    # pp p.a.
    d_rev_pa   = d_earn_pa * eps,                             # pp p.a.
    annual_m   = d_rev_pa / 100 * ANCHOR$base_for_fiscal,     # GBP m p.a.
    terminal_m = 10 * annual_m,                               # yr-10 flow
    cumul_m    = 55 * annual_m,                               # sum yrs 1-10
    pct_netpos_annual   = 100 * annual_m   / ANCHOR$net_position_m,
    pct_netpos_terminal = 100 * terminal_m / ANCHOR$net_position_m)
save_csv(fiscal, file.path(PATHS$tables, "fiscal_translation.csv"))

# LaTeX macro definitions for preamble
g <- function(sc, col) fiscal[[col]][match(sc, fiscal$scenario)]
r3 <- function(x) formatC(abs(x), format = "f", digits = 4)
r1 <- function(x) formatC(abs(x), format = "f", digits = 1)
r0 <- function(x) formatC(abs(x), format = "f", digits = 0)

cat(sprintf(paste0(
  "\\renewcommand{\\varepsilonNSND}{%.2f}\n",
  "\\renewcommand{\\revLo}{%s}\n\\renewcommand{\\revCen}{%s}\n\\renewcommand{\\revHi}{%s}\n",
  "\\renewcommand{\\fiscLo}{%s}\n\\renewcommand{\\fiscCen}{%s}\n\\renewcommand{\\fiscHi}{%s}\n",
  "\\renewcommand{\\termLo}{%s}\n\\renewcommand{\\termCen}{%s}\n\\renewcommand{\\termHi}{%s}\n",
  "\\renewcommand{\\cumLo}{%s}\n\\renewcommand{\\cumCen}{%s}\n\\renewcommand{\\cumHi}{%s}\n",
  "\\renewcommand{\\fiscPctTerm}{%.1f}\n"),
  eps,
  r3(g("lo","d_rev_pa")),   r3(g("central","d_rev_pa")),   r3(g("hi","d_rev_pa")),
  r1(g("lo","annual_m")),   r1(g("central","annual_m")),   r1(g("hi","annual_m")),
  r0(g("lo","terminal_m")), r0(g("central","terminal_m")), r0(g("hi","terminal_m")),
  r0(g("lo","cumul_m")),    r0(g("central","cumul_m")),    r0(g("hi","cumul_m")),
  abs(g("central","pct_netpos_terminal"))))
