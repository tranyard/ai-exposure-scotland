# Industry margin of Assumption 1. Exposure plausibly varies within an
# occupation across industries, and a single E_uk_k conditions that
# variation out; region invariance fails on this margin exactly where the
# within-occupation industry mix differs across the border in a way
# correlated with exposure.
#
# For each SOC3 minor group k this computes the total-variation distance
# between the Scottish and rUK within-occupation industry distributions,
#   TV_k = 0.5 * sum_i | p(i | k, Scot) - p(i | k, rUK) |,
# the Scotland-employment-weighted mean B = sum_k sigma_k,Scot TV_k
# (if within-occupation cross-industry exposure spans at most delta index
# points then |bias in dE_bar| <= delta * B), weighted correlations of
# TV_k with exposure and with each cell's |contribution| to the gap, and
# the most divergent cells.
source(here::here("R", "00_config.R"))

V <- APS$vars
NMIN <- 30L        # per-region respondent floor for a cell's TV to count

fs <- list.files(PATHS$aps, pattern = "^aps_.*\\.csv$", full.names = TRUE)
if (!length(fs)) stop("No APS microdata in ", PATHS$aps, call. = FALSE)
micro <- purrr::map_dfr(fs, \(f)
                        readr::read_csv(f, show_col_types = FALSE,
                                        col_types = readr::cols(.default = readr::col_character())))
if (!V$industry %in% names(micro))
  stop("Industry variable '", V$industry, "' not on the APS file.", call. = FALSE)

base <- micro |>
  mutate(ilo      = aps_num(.data[[V$ilo]]),
         country  = aps_num(.data[[V$country]]),
         soc_num  = aps_num(.data[[V$soc]]),
         industry = aps_num(.data[[V$industry]]),
         year     = as.integer(aps_num(.data[[V$year]])),
         w        = aps_num(.data[[V$weight]])) |>
  filter(ilo == APS$codes$in_employment, !is.na(industry)) |>
  mutate(region = region_of(country),
         soc_uk = substr(as.character(as.integer(soc_num)), 1, APS$soc_level),
         ind    = as.character(as.integer(industry))) |>
  filter(!is.na(region), !is.na(soc_uk), nzchar(soc_uk),
         !is.na(w), w > 0, year %in% APS$years)

occ_ind <- base |>
  group_by(soc_uk, region, ind) |>
  summarise(l = sum(w), n = n(), .groups = "drop") |>
  group_by(soc_uk, region) |>
  mutate(p = l / sum(l), n_cell = sum(n)) |>
  ungroup()

tv <- occ_ind |>
  select(soc_uk, region, ind, p, n_cell) |>
  pivot_wider(names_from = region, values_from = c(p, n_cell),
              values_fill = list(p = 0)) |>
  group_by(soc_uk) |>
  summarise(TV        = 0.5 * sum(abs(p_Scotland - p_rUK)),
            n_Scot    = first(na.omit(n_cell_Scotland)),
            n_rUK     = first(na.omit(n_cell_rUK)),
            .groups   = "drop") |>
  mutate(adequate = !is.na(n_Scot) & !is.na(n_rUK) &
           n_Scot >= NMIN & n_rUK >= NMIN)

aps <- read_csv(file.path(PATHS$cache, "aps_pool.csv"), show_col_types = FALSE) |>
  mutate(soc_uk = as.character(soc_uk))
sig <- aps |> select(soc_uk, region, sigma) |>
  pivot_wider(names_from = region, values_from = sigma,
              values_fill = list(sigma = 0), names_prefix = "sigma_")
uk  <- read_csv(file.path(PATHS$cache, "uk_soc3_scores.csv"),
                show_col_types = FALSE) |>
  mutate(soc_uk = as.character(soc_uk)) |> select(soc_uk, E_uk)
dec_f <- table_csv("gap_decomposition")
dec <- if (file.exists(dec_f)) {
  read_csv(dec_f, show_col_types = FALSE) |>
    mutate(soc_uk = as.character(soc_uk)) |>
    select(soc_uk, contribution)
} else {
  tibble(soc_uk = character(), contribution = numeric())
}

d <- tv |>
  inner_join(sig, by = "soc_uk") |>
  inner_join(uk,  by = "soc_uk") |>
  left_join(dec,  by = "soc_uk") |>
  filter(adequate)

wcor <- function(x, y, w) {
  w <- w / sum(w)
  cx <- x - sum(w * x); cy <- y - sum(w * y)
  sum(w * cx * cy) / sqrt(sum(w * cx^2) * sum(w * cy^2))
}

B_scot <- sum(d$sigma_Scotland * d$TV) / sum(d$sigma_Scotland)
B_raw  <- sum(d$sigma_Scotland * d$TV)
cov_S  <- sum(d$sigma_Scotland)

summary_tbl <- tibble(
  n_cells_adequate     = nrow(d),
  emp_coverage_Scot    = cov_S,
  TV_mean_Scot_wt      = B_scot,
  TV_median            = median(d$TV),
  TV_p90               = quantile(d$TV, 0.9),
  bound_multiplier_B   = B_raw,
  bias_bound_delta5    = 5  * B_raw,
  bias_bound_delta10   = 10 * B_raw,
  corr_TV_exposure     = wcor(d$TV, d$E_uk, d$sigma_Scotland),
  corr_TV_abs_contrib  = if (all(is.na(d$contribution))) NA_real_
  else wcor(d$TV, abs(d$contribution),
            d$sigma_Scotland)
)
save_csv(summary_tbl, file.path(PATHS$cache, "industry_invariance_raw.csv"))

# TV_mean_Scot_wt and bound_multiplier_B differ only by the coverage
# factor; only the latter enters the bias bound, so only it is printed,
# with the coverage caveat carried in the table note.
save_csv(tibble(
  Statistic = c("Minor groups with adequate cells",
                "Share of Scottish employment covered",
                "Median TV", "90th percentile TV",
                "Bias bound multiplier B",
                "Implied bound on gap, delta = 5 index points",
                "Correlation of TV with exposure",
                "Correlation of TV with |gap contribution|"),
  Value = c(summary_tbl$n_cells_adequate, summary_tbl$emp_coverage_Scot,
            summary_tbl$TV_median, summary_tbl$TV_p90,
            summary_tbl$bound_multiplier_B, summary_tbl$bias_bound_delta5,
            summary_tbl$corr_TV_exposure, summary_tbl$corr_TV_abs_contrib)),
  file.path(PATHS$tables, "industry_invariance_summary.csv"))
save_csv(d |> arrange(desc(TV)),
         file.path(PATHS$tables, "industry_tv_by_occupation.csv"))

message(sprintf(paste0(
  "industry margin: %d adequate cells (%.0f%% of Scottish employment) | ",
  "weighted mean TV = %.3f (median %.3f, p90 %.3f) | bound %.3f pts at delta = 5 | ",
  "corr(TV, E_uk) = %.2f | corr(TV, |contribution|) = %.2f"),
  nrow(d), 100 * cov_S, B_scot, summary_tbl$TV_median, summary_tbl$TV_p90,
  summary_tbl$bias_bound_delta5,
  summary_tbl$corr_TV_exposure, summary_tbl$corr_TV_abs_contrib))
