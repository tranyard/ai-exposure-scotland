# Run the pipeline in dependency order. Stages that cost API credit (04,
# 11, 14) run only with RUN_SCORING=1; the analysis re-runs cheaply from
# cached scores.
#
#   Rscript R/_run_all.R                 analysis only
#   RUN_SCORING=1 Rscript R/_run_all.R   also (re)run the LLM scoring
RUN_SCORING <- nzchar(Sys.getenv("RUN_SCORING"))
root <- here::here()
run <- function(f) { message("\n===== ", f, " ====="); source(file.path(root, "R", f)) }

run("01_prepare_onet.R")
run("02_prompt.R")

if (RUN_SCORING) run("04_run_full_scoring.R")
run("05_aggregate_occupation.R")
if (RUN_SCORING) run("11_calibration_experiment.R")

run("06_crosswalk.R")
run("07_aps_employment.R")
run("08_region_indices_shiftshare.R")

run("16_comparators.R")
run("09_employment_regressions.R")
run("13_css.R")
run("12_montecarlo.R")
run("10_acemoglu_projection.R")
run("10b_acemoglu_panelc.R")

if (RUN_SCORING) run("14_common_mode_test.R")
run("15_wage_eiv.R")
run("20_elasticity.R")

run("17_resolution_robustness.R")
run("18_industry_decomposition.R")
run("19_industry_invariance.R")

message("\nPipeline complete. Tables (.tex) in tables/, csv mirrors in ",
        "out/cache/tables_csv/, figures in figures/.")
