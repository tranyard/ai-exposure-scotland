# =====================================================================
# _run_all.R  —  Run the pipeline in the revised plan's order.
# Stages that cost API credit (04, 11, 14) are gated behind RUN_SCORING.
# The non-scoring analysis re-runs cheaply from cached CSVs.
#
#   Rscript R/_run_all.R            # analysis only (assumes scores exist)
#   RUN_SCORING=1 Rscript R/_run_all.R   # also (re)run the LLM scoring
# =====================================================================
RUN_SCORING <- nzchar(Sys.getenv("RUN_SCORING"))
root <- here::here()
run <- function(f) { message("\n===== ", f, " ====="); source(file.path(root, "R", f)) }

# --- Implementation order (Plan §2.10) -------------------------------
run("01_prepare_onet.R")              # task dataset
run("02_prompt.R")                    # freeze + hash prompt

if (RUN_SCORING) run("04_run_full_scoring.R")   # step 3a: full baseline (Batch API)
run("05_aggregate_occupation.R")      # occupation composites + classification
if (RUN_SCORING) run("11_calibration_experiment.R")  # step 2: sigma^2_g, cross-model bias

run("06_crosswalk.R")                 # O*NET -> UK SOC 2020
run("07_aps_employment.R")            # APS weights
run("08_region_indices_shiftshare.R") # step 4: indices + shift-share
run("09_employment_regressions.R")    # employment regressions
run("13_css.R")                       # step 5b: classification stability
run("12_montecarlo.R")                # step 5a: credible intervals
run("10_acemoglu_projection.R")       # step 6: productivity + GDP

if (RUN_SCORING) run("14_common_mode_test.R")   # step 3b: cross-model robustness
run("15_wage_eiv.R")                  # step 7: conditional on ASHE

message("\nPipeline complete. Tables in out/tables, figures in out/figures.")
