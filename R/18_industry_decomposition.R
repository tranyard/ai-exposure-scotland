# The industry margin. Exposure attaches to occupations only, so the
# design conditions out occupation-by-industry variation in exposure by
# construction. What it can ask is whether the compositional gap runs
# through Scotland's industry mix or through its occupational mix within
# industries. With sigma_{k,i,r} the SOC3 x SIC-section employment share,
# sigma_{i,r} the industry share, and Ebar_{i,r} = sum_k sigma_{k|i,r}
# E_k / 100 the within-industry occupational exposure,
#   dEbar = sum_i dsigma_i Ebar_{i,rUK}   (industry mix)
#         + sum_i sigma_{i,Scot} dEbar_i  (within industry)
# is exact. The split is not unique: the reversed weighting is equally
# valid, so both orderings are reported.
#
# Requires aps_occind_pool.csv from 07_aps_employment.R.
source(here::here("R", "00_config.R"))

f <- file.path(PATHS$cache, "aps_occind_pool.csv")
if (!file.exists(f))
  stop("aps_occind_pool.csv missing - run 07_aps_employment.R ",
       "(and check ", APS$vars$industry, " is on the APS files).", call. = FALSE)

occind <- read_csv(f, show_col_types = FALSE) |>
  mutate(soc_uk = as.character(soc_uk),
         industry = as.integer(industry),
         section  = ifelse(industry >= 1 & industry <= 26,
                           LETTERS[industry], as.character(industry)))
uk <- read_csv(file.path(PATHS$cache, "uk_soc3_scores.csv"),
               show_col_types = FALSE) |>
  transmute(soc_uk = as.character(soc_uk), E = E_uk)

panel <- occind |> inner_join(uk, by = "soc_uk")
cov <- panel |> group_by(region) |>
  summarise(cov = sum(sigma), .groups = "drop")
message("employment coverage after score match: ",
        paste(sprintf("%s %.1f%%", cov$region, 100 * cov$cov), collapse = " | "))

by_ind <- panel |>
  group_by(region, section) |>
  summarise(l = sum(l_pool),
            Ebar_i = sum(l_pool * E / 100) / sum(l_pool), .groups = "drop") |>
  group_by(region) |>
  mutate(sigma_i = l / sum(l)) |>
  ungroup()

wide <- by_ind |>
  select(section, region, sigma_i, Ebar_i) |>
  pivot_wider(names_from = region, values_from = c(sigma_i, Ebar_i),
              values_fill = list(sigma_i = 0)) |>
  # A section absent from one region has no within term there; carry the
  # other region's mean so the identity still sums exactly.
  mutate(Ebar_i_Scotland = coalesce(Ebar_i_Scotland, Ebar_i_rUK),
         Ebar_i_rUK      = coalesce(Ebar_i_rUK, Ebar_i_Scotland),
         dsigma_i = sigma_i_Scotland - sigma_i_rUK,
         dEbar_i  = Ebar_i_Scotland - Ebar_i_rUK,
         between_A = dsigma_i * Ebar_i_rUK,
         within_A  = sigma_i_Scotland * dEbar_i,
         between_B = dsigma_i * Ebar_i_Scotland,
         within_B  = sigma_i_rUK * dEbar_i)

tot <- wide |> summarise(
  between_A = sum(between_A), within_A = sum(within_A),
  between_B = sum(between_B), within_B = sum(within_B)) |>
  mutate(total_A = between_A + within_A,
         total_B = between_B + within_B)

# Demeaned between-section terms for the per-section table. Sum-
# preserving, since sum_i dsigma_i = 0. Undemeaned, a section's between
# term is dominated by the level of Ebar_i_r rather than by how unusual
# that section's Scotland-rUK share gap is.
Ebar_rUK_bar  <- with(wide, sum(sigma_i_rUK      * Ebar_i_rUK))
Ebar_Scot_bar <- with(wide, sum(sigma_i_Scotland * Ebar_i_Scotland))
wide <- wide |>
  mutate(between_A_demeaned = dsigma_i * (Ebar_i_rUK      - Ebar_rUK_bar),
         between_B_demeaned = dsigma_i * (Ebar_i_Scotland - Ebar_Scot_bar))
stopifnot(abs(sum(wide$between_A_demeaned) - tot$between_A) < 1e-8,
          abs(sum(wide$between_B_demeaned) - tot$between_B) < 1e-8)

gap_chk <- panel |> group_by(region) |>
  summarise(E_bar = sum(sigma * E / 100) / sum(sigma), .groups = "drop") |>
  pivot_wider(names_from = region, values_from = E_bar) |>
  transmute(gap = Scotland - rUK) |> pull(gap)
stopifnot(abs(tot$total_A - gap_chk) < 1e-8,
          abs(tot$total_B - gap_chk) < 1e-8)

save_csv(tot |> mutate(gap_this_sample = gap_chk),
         file.path(PATHS$cache, "industry_decomposition_raw.csv"))

# Contributions are scaled x100 to index points, matching the headline
# gap; E_uk enters Ebar as E/100 above, so tot is on the 0-1 scale.
save_csv(tibble(
  Term = c("Industry mix", "Within industry", "Total (= gap)"),
  `Ordering A (rUK exposure, Scottish shares)` =
    100 * c(tot$between_A, tot$within_A, tot$total_A),
  `Ordering B (Scottish exposure, rUK shares)` =
    100 * c(tot$between_B, tot$within_B, tot$total_B)),
  file.path(PATHS$tables, "industry_decomposition.csv"))
full_contrib <- wide |> arrange(within_A) |>
  select(section, sigma_i_Scotland, sigma_i_rUK, dsigma_i,
         Ebar_i_Scotland, Ebar_i_rUK, dEbar_i,
         between_A_demeaned, within_A, between_B_demeaned, within_B)
save_csv(full_contrib, file.path(PATHS$tables, "industry_contributions_full.csv"))

sic_section_name <- c(
  A = "Agriculture, forestry \\& fishing", B = "Mining \\& quarrying",
  C = "Manufacturing",              D = "Energy supply",
  E = "Water \\& waste",             F = "Construction",
  G = "Wholesale \\& retail",        H = "Transport \\& storage",
  I = "Accommodation \\& food",      J = "Information \\& communication",
  K = "Finance \\& insurance",       L = "Real estate",
  M = "Professional \\& technical",  N = "Administrative \\& support",
  O = "Public admin \\& defence",    P = "Education",
  Q = "Health \\& social work",      R = "Arts \\& recreation",
  S = "Other services",             T = "Household employers",
  U = "Extraterritorial bodies")

# Printed version: ordering A only, sorted by the within-industry
# exposure gap so the table is spined by its own mechanism. Contribution
# and exposure-gap columns are x100 to index points and the share-gap
# column x100 to percentage points.
save_csv(full_contrib |>
           arrange(dEbar_i) |>
           transmute(Section = section,
                     Industry = unname(sic_section_name[section]),
                     `Share gap (Scot - rUK, pp)` = 100 * dsigma_i,
                     `Within-industry exposure gap` = 100 * dEbar_i,
                     `Industry-mix contribution` = 100 * between_A_demeaned,
                     `Within-industry contribution` = 100 * within_A),
         file.path(PATHS$tables, "industry_contributions.csv"))

message(sprintf(paste0(
  "gap on this sample: %+.4f | ordering A: mix %+.4f, within %+.4f",
  " | ordering B: mix %+.4f, within %+.4f"),
  gap_chk, tot$between_A, tot$within_A, tot$between_B, tot$within_B))
