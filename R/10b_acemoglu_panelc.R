# Panel C: the TFP projection with the classified task-content share

source(here::here("R", "00_config.R"))

# soc_uk is created as character in 06 but readr type-guesses it back as
# a double from CSV. This script joins across three separate reads, so
# every key is normalised immediately after reading.
norm_soc <- function(x) {
  chr <- if (is.numeric(x)) format(x, scientific = FALSE, trim = TRUE)
  else as.character(x)
  substr(gsub("[^0-9]", "", chr), 1, APS$soc_level)
}

# Central overall cost saving, Acemoglu (2024): 0.27 (average labour
# cost saving) x 0.57 (UK labour share) = 0.154. See 10_acemoglu_projection.R
# for the low/central/high derivation.
KAPPA_CENTRAL <- 0.154

panel <- read_csv(require_file(
  file.path(PATHS$cache, "region_panel.csv"),
  "Run the script that builds region_panel.csv first."),
  show_col_types = FALSE) |>
  mutate(soc_uk = norm_soc(soc_uk))

# Same source and fallback as 10_acemoglu_projection.R, built after panel
# so the fallback inherits the normalised key type.
pi_k <- tryCatch(
  read_csv(file.path(PATHS$sfc, "task_cost_share.csv"), show_col_types = FALSE) |>
    mutate(soc_uk = norm_soc(soc_uk)),
  error = function(e) distinct(panel, soc_uk) |> mutate(pi = 0.23))

read_share <- function(set) {
  suffix <- if (set == "central") "" else paste0("_", set)
  f <- file.path(PATHS$cache, paste0("uk_soc3_scores", suffix, ".csv"))
  require_file(f, paste0("Panel C needs the '", set,
                         "' threshold crosswalk output; re-run 06_crosswalk.R."))
  d <- read_csv(f, show_col_types = FALSE)
  missing <- setdiff(c("soc_uk", "Sub_uk", "Comp_uk"), names(d))
  if (length(missing))
    stop("uk_soc3_scores", suffix, ".csv is missing column(s): ",
         paste(missing, collapse = ", "),
         ". Re-run 05_aggregate_occupation.R then 06_crosswalk.R.", call. = FALSE)
  d |>
    mutate(soc_uk = norm_soc(soc_uk),
           Share_k = Sub_uk + Comp_uk,
           thresholds = set) |>
    select(soc_uk, Share_k, thresholds)
}

shares <- bind_rows(lapply(names(THRESHOLDS), read_share))

base <- panel |>
  select(soc_uk, region, sigma) |>
  distinct() |>
  left_join(pi_k, by = "soc_uk") |>
  mutate(pi = coalesce(pi, 0.23))

sig_chk <- base |> group_by(region) |> summarise(s = sum(sigma), .groups = "drop")


# One threshold set at a time: each is a clean many-to-one join, which
# avoids a many-to-many warning.
aggregate_set <- function(set) {
  s <- shares |> filter(thresholds == set) |> select(soc_uk, Share_k)
  j <- base |> inner_join(s, by = "soc_uk")

  lost <- n_distinct(base$soc_uk) - n_distinct(j$soc_uk)
  if (lost > 0)
    warning(sprintf(
      "Panel C [%s]: %d of %d occupations found no match in uk_soc3_scores.",
      set, lost, n_distinct(base$soc_uk)), call. = FALSE)

  j |>
    group_by(region) |>
    summarise(A = sum(sigma * pi * Share_k),
              Share_bar = sum(sigma * Share_k),
              .groups = "drop") |>
    mutate(thresholds = set)
}

A <- bind_rows(lapply(names(THRESHOLDS), aggregate_set))
n_expected <- length(names(THRESHOLDS)) * n_distinct(base$region)
if (nrow(A) != n_expected)
  warning(sprintf("Panel C: got %d region x threshold cells, expected %d.",
                  nrow(A), n_expected), call. = FALSE)

proj <- A |> mutate(kappa = KAPPA_CENTRAL, dTFP_pct = 100 * kappa * A)
save_csv(proj, file.path(PATHS$tables, "acemoglu_panelc.csv"))

diff_tbl <- proj |>
  select(region, thresholds, dTFP_pct) |>
  pivot_wider(names_from = region, values_from = dTFP_pct) |>
  mutate(diff = Scotland - rUK,
         thresholds = factor(thresholds, levels = c("low", "central", "high"))) |>
  arrange(thresholds) |>
  mutate(thresholds = as.character(thresholds))
save_csv(diff_tbl, file.path(PATHS$tables, "acemoglu_panelc_differential.csv"))

lbl <- vapply(names(THRESHOLDS), function(s)
  sprintf("(%d,%d)", as.integer(THRESHOLDS[[s]]["sub"]),
          as.integer(THRESHOLDS[[s]]["comp"])), character(1))
for (i in seq_len(nrow(diff_tbl))) {
  set <- diff_tbl$thresholds[i]
  message(sprintf("%-10s rUK %.3f | Scotland %.3f | diff %+.4f",
                  lbl[[set]], diff_tbl$rUK[i], diff_tbl$Scotland[i],
                  diff_tbl$diff[i]))
}

sgn <- sign(diff_tbl$diff)
