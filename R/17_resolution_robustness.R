# Occupational-resolution robustness.
#
# Part A recomputes the differential at four-digit unit groups. It needs
# a SOC 2020 unit-group variable on the APS microdata, which the public
# EUL file does not carry (occupation resolves only to the minor group),
# so it runs on the secure-access extract:
#     APS_SOC4_VAR=SOC20M Rscript R/17_resolution_robustness.R
# Otherwise it reports the skip and Part B runs alone.
#
# Part B varies the crosswalk aggregation rule. The baseline (06 /
# uk3_continuous) is a concordance-weighted mean O*NET -> SOC4 then an
# unweighted mean SOC4 -> SOC3, no domestic weight existing below the
# minor group on public data. Alternatives: equal weights at both steps;
# mapping-mass weights at step 2; a direct one-step weighted mean; and UK
# SOC4 employment weights at step 2 (needs Part A).
source(here::here("R", "00_config.R"))

aps3 <- read_csv(file.path(PATHS$cache, "aps_pool.csv"), show_col_types = FALSE) |>
  mutate(soc_uk = as.character(soc_uk))
occ <- read_csv(file.path(PATHS$cache, "occupation_scores_V0.csv"),
                show_col_types = FALSE) |>
  mutate(onet_soc_code = as.character(onet_soc_code))
map <- read_uk_onet_map()
if (!"weight" %in% names(map)) map$weight <- 1
map <- map |>
  mutate(soc4 = substr(gsub("[^0-9]", "", soc_uk4), 1, 4),
         soc_uk = substr(soc4, 1, APS$soc_level))

# sigma is renormalised over the matched cells, so every variant is an
# index over its own covered support; coverage is reported alongside.
gap_of <- function(shares, scores, soc_col = "soc_uk") {
  shares |>
    inner_join(scores, by = setNames("soc", soc_col)) |>
    group_by(region) |>
    summarise(E_bar = sum(sigma * E / 100) / sum(sigma), .groups = "drop") |>
    pivot_wider(names_from = region, values_from = E_bar) |>
    transmute(Scotland, rUK, gap = Scotland - rUK)
}

SOC4_VAR <- Sys.getenv("APS_SOC4_VAR", "SOC20M")
V <- APS$vars

aps_files <- list.files(PATHS$aps, pattern = "^aps_micro_.*\\.csv$", full.names = TRUE)
peek <- if (length(aps_files)) names(readr::read_csv(aps_files[1], n_max = 0,
                                                     show_col_types = FALSE)) else character()
has_soc4 <- SOC4_VAR %in% peek

if (!has_soc4) {
  message("Part A skipped: '", SOC4_VAR, "' not on the APS microdata.")
} else {
  micro <- purrr::map_dfr(aps_files, \(f)
    readr::read_csv(f, show_col_types = FALSE,
                    col_types = readr::cols(.default = readr::col_character())))
  base4 <- micro |>
    mutate(ilo     = aps_num(.data[[V$ilo]]),
           country = aps_num(.data[[V$country]]),
           soc4    = substr(gsub("[^0-9]", "", as.character(.data[[SOC4_VAR]])), 1, 4),
           year    = as.integer(aps_num(.data[[V$year]])),
           w       = aps_num(.data[[V$weight]])) |>
    filter(ilo == APS$codes$in_employment, !is.na(w), w > 0,
           nchar(soc4) == 4, year %in% APS$years) |>
    mutate(region = region_of(country))

  pool4 <- base4 |>
    group_by(soc4, region) |>
    summarise(l_pool = sum(w), n_unw = n(), .groups = "drop") |>
    group_by(region) |>
    mutate(sigma = l_pool / sum(l_pool)) |>
    ungroup()

  # Thin Scottish SOC4 cells are the reason the headline sits at SOC3.
  diag4 <- pool4 |> filter(region == "Scotland") |>
    summarise(cells = n(),
              thin_lt30  = sum(n_unw < 30),
              thin_lt100 = sum(n_unw < 100),
              min_n      = min(n_unw))
  message(sprintf("Scotland SOC4 cells: %d | n<30: %d | n<100: %d | smallest: %d",
                  diag4$cells, diag4$thin_lt30, diag4$thin_lt100, diag4$min_n))

  uk4 <- read_csv(file.path(PATHS$cache, "uk_soc4_scores.csv"),
                  show_col_types = FALSE) |>
    transmute(soc4 = substr(gsub("[^0-9]", "", soc_uk4), 1, 4), E = E_uk) |>
    group_by(soc4) |> summarise(E = mean(E), .groups = "drop")

  g4 <- gap_of(pool4 |> rename(soc = soc4), uk4 |> rename(soc = soc4), "soc")
  cov4 <- pool4 |> mutate(m = soc4 %in% uk4$soc4) |>
    group_by(region) |> summarise(cov = sum(sigma[m]), .groups = "drop")

  # SOC3 gap on the same sample, so any SOC4-vs-SOC3 difference is
  # resolution rather than sample composition.
  pool3s <- base4 |>
    mutate(soc_uk = substr(soc4, 1, APS$soc_level)) |>
    group_by(soc_uk, region) |>
    summarise(l_pool = sum(w), .groups = "drop") |>
    group_by(region) |> mutate(sigma = l_pool / sum(l_pool)) |> ungroup()
  uk3 <- read_csv(file.path(PATHS$cache, "uk_soc3_scores.csv"),
                  show_col_types = FALSE) |>
    transmute(soc = as.character(soc_uk), E = E_uk)
  g3s <- gap_of(pool3s |> rename(soc = soc_uk), uk3, "soc")

  out_a <- tibble(level = c("SOC4", "SOC3 (same sample)"),
                  Scotland = c(g4$Scotland, g3s$Scotland),
                  rUK      = c(g4$rUK, g3s$rUK),
                  gap      = c(g4$gap, g3s$gap),
                  scot_cells = c(diag4$cells, NA),
                  scot_thin_lt30 = c(diag4$thin_lt30, NA),
                  emp_coverage_scot = c(cov4$cov[cov4$region == "Scotland"], NA))
  save_csv(out_a |> transmute(
             `Working level`             = level,
             Scotland, rUK, Gap = gap,
             `Scottish cells`            = scot_cells,
             `Cells with n < 30`         = scot_thin_lt30,
             `Scottish employment covered` = emp_coverage_scot),
           file.path(PATHS$tables, "soc4_differential.csv"))
  message(sprintf("SOC4 gap %+.4f vs SOC3 gap %+.4f on the same sample",
                  g4$gap, g3s$gap))

  save_csv(pool4 |> group_by(soc4) |> summarise(l = sum(l_pool), .groups = "drop"),
           file.path(PATHS$cache, "aps_soc4_uk_emp.csv"))
}

# Step 1: O*NET -> SOC4; step 2: SOC4 -> SOC3 (skipped under "direct").
agg_rule <- function(w1 = c("weight", "equal"), w2 = c("equal", "mass", "uk_emp"),
                     direct = FALSE) {
  m <- map |> inner_join(occ |> select(onet_soc_code, E_j), by = "onet_soc_code")
  m$w1v <- if (match.arg(w1) == "weight") m$weight else 1
  if (direct) {
    return(m |> group_by(soc_uk) |>
             summarise(E = weighted.mean(E_j, w1v), .groups = "drop"))
  }
  s4 <- m |> group_by(soc_uk, soc4) |>
    summarise(E4 = weighted.mean(E_j, w1v),
              mass = sum(weight), .groups = "drop")
  w2 <- match.arg(w2)
  if (w2 == "uk_emp") {
    f <- file.path(PATHS$cache, "aps_soc4_uk_emp.csv")
    if (!file.exists(f)) return(NULL)     # needs Part A
    s4 <- s4 |> inner_join(read_csv(f, show_col_types = FALSE), by = "soc4")
    s4$w2v <- s4$l
  } else s4$w2v <- if (w2 == "mass") s4$mass else 1
  s4 |> group_by(soc_uk) |>
    summarise(E = weighted.mean(E4, w2v), .groups = "drop")
}

rules <- list(
  baseline = agg_rule("weight", "equal"),
  equal    = agg_rule("equal",  "equal"),
  mass     = agg_rule("weight", "mass"),
  direct   = agg_rule("weight", direct = TRUE),
  uk_emp   = agg_rule("weight", "uk_emp"))

sens <- imap_dfr(purrr::compact(rules), \(sc, nm) {
  g <- gap_of(aps3 |> rename(soc = soc_uk), sc |> rename(soc = soc_uk), "soc")
  tibble(rule = nm, Scotland = g$Scotland, rUK = g$rUK, gap = g$gap)
})
rule_label <- c(
  baseline = "Baseline (weighted, then unweighted)",
  equal    = "Equal weights at both steps",
  mass     = "Mapping-mass weights at step 2",
  direct   = "Direct one-step weighted mean",
  uk_emp   = "UK employment weights at step 2")
save_csv(sens |> transmute(
           `Aggregation rule` = unname(rule_label[rule]),
           Scotland, rUK, Gap = gap),
         file.path(PATHS$tables, "crosswalk_aggregation_sensitivity.csv"))
message("crosswalk aggregation sensitivity: ",
        paste(sprintf("%s %+.4f", sens$rule, sens$gap), collapse = " | "))

# The baseline rule must reproduce the headline gap.
hl_f <- table_csv("region_indices_continuous")
if (file.exists(hl_f)) {
  hl <- read_csv(hl_f, show_col_types = FALSE) |>
    filter(operator == "max") |> pull(gap)
  drift <- abs(sens$gap[sens$rule == "baseline"] - hl)
  if (is.finite(drift) && drift > 1e-6)
    warning(sprintf("baseline rule drifts from the headline by %.2e - ",
                    drift), "check crosswalk/scores staleness.", call. = FALSE)
}
