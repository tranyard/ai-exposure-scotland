source(here::here("R", "00_config.R"))

panel <- read_csv(file.path(PATHS$cache, "region_panel.csv"), show_col_types = FALSE)

# kappa band and task-cost shares from the OBR/Acemoglu calibration; fall
# back to a placeholder band and pi_k = 1 until the SFC files are supplied.
calib <- tryCatch(read_csv(file.path(PATHS$sfc, "acemoglu_calibration.csv"),
                           show_col_types = FALSE),
                  error = function(e)
                    tibble(scenario = c("lo", "central", "hi"), kappa = c(0.11, 0.144, 0.27)))
pi_k <- tryCatch(read_csv(file.path(PATHS$sfc, "task_cost_share.csv"), show_col_types = FALSE),
                 error = function(e) distinct(panel, soc_uk) |> mutate(pi = 0.23))

base <- panel |>
  left_join(pi_k, by = "soc_uk") |>
  mutate(pi = coalesce(pi, 1),
         composite = E_uk / 100, substitution = E_uk_sub / 100,
         complementarity = E_uk_comp / 100)

A <- base |>
  pivot_longer(c(composite, substitution, complementarity),
               names_to = "channel", values_to = "Ebar") |>
  group_by(region, channel) |>
  summarise(A = sum(sigma * pi * Ebar), .groups = "drop")

proj <- crossing(A, calib) |> mutate(dTFP_pct = 100 * kappa * A)
save_csv(proj, file.path(PATHS$tables, "acemoglu_projection.csv"))

diff_tbl <- proj |>
  select(region, channel, scenario, dTFP_pct) |>
  pivot_wider(names_from = region, values_from = dTFP_pct) |>
  mutate(diff = Scotland - rUK)
save_csv(diff_tbl, file.path(PATHS$tables, "acemoglu_differential.csv"))

diff_tbl |> filter(scenario == "central") |>
  mutate(line = sprintf("%s: %.3f pp", channel, diff)) |> pull(line) |>
  paste(collapse = " | ") |> message()

# GDP translation if the SFC pass-through file is present.
gdp <- tryCatch({
  g <- read_csv(file.path(PATHS$sfc, "gdp_passthrough.csv"), show_col_types = FALSE)
  proj |> filter(channel == "composite") |>
    inner_join(g, by = "region") |>
    mutate(dGDP = gva_base * labour_share * kappa * A)
}, error = function(e) NULL)
if (!is.null(gdp)) save_csv(gdp, file.path(PATHS$tables, "gdp_projection.csv"))
