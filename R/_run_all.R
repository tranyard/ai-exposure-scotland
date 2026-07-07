# =====================================================================
# _run_all.R  —  Run the pipeline in dependency order.
# Stages that cost API credit (04, 11, 14) are gated behind RUN_SCORING.
# The non-scoring analysis re-runs cheaply from cached CSVs.
#
#   Rscript R/_run_all.R                 # analysis only (assumes scores exist)
#   RUN_SCORING=1 Rscript R/_run_all.R   # also (re)run the LLM scoring
#
# Order note: the calibration experiment (11) runs AFTER the full V0
# scoring, because its sample is stratified on realised scores and its
# V0 arm is reused from the full run. All batch stages are resumable:
# re-running after an interruption resumes the stored batch rather than
# resubmitting it.
# =====================================================================
RUN_SCORING <- nzchar(Sys.getenv("RUN_SCORING"))
root <- here::here()
run <- function(f) { message("\n===== ", f, " ====="); source(file.path(root, "R", f)) }

run("01_prepare_onet.R")              # task dataset
run("02_prompt.R")                    # freeze + hash prompt (system + user)

if (RUN_SCORING) run("04_run_full_scoring.R")   # full baseline V0 (Batch API)
run("05_aggregate_occupation.R")      # composites + classification (all thr sets)
if (RUN_SCORING) run("11_calibration_experiment.R")  # sigma^2_g, sensitivities, flips

run("06_crosswalk.R")                 # O*NET -> UK SOC 2020 (all thr sets)
run("07_aps_employment.R")            # APS weights
run("08_region_indices_shiftshare.R") # indices + shift-share + thr sensitivity
run("09_employment_regressions.R")    # employment regressions
run("13_css.R")                       # task-level classification stability
run("12_montecarlo.R")                # credible intervals (continuous index)
run("10_acemoglu_projection.R")       # productivity + GDP (both channels)

if (RUN_SCORING) run("14_common_mode_test.R")   # cross-model robustness (M', M'')
run("15_wage_eiv.R")                  # conditional on ASHE

message("\nPipeline complete. Tables in out/tables, figures in out/figures.")
