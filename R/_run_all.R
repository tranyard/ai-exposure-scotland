# =====================================================================
# _run_all.R  —  Run the pipeline in dependency order.
# Stages that cost API credit (04, 11, 14)
# The non-scoring analysis re-runs cheaply from CSVs
#
# Terminal commands:
#   Rscript R/_run_all.R                 # analysis only (assumes scores exist)
#   RUN_SCORING=1 Rscript R/_run_all.R   # also (re)run the LLM scoring
#
# CHANGES: 16_comparators.R added after 08. Note 14 now defaults to the
# Haiku arm (CM_ARM=Mpp); set CM_ARM=Mp or BOTH explicitly for GPT-4o.
# =====================================================================
RUN_SCORING <- nzchar(Sys.getenv("RUN_SCORING"))
root <- here::here()
run <- function(f) { message("\n===== ", f, " ====="); source(file.path(root, "R", f)) }

run("01_prepare_onet.R")              # task dataset
run("02_prompt.R")                    # freeze + hash prompt (system + user)

if (RUN_SCORING) run("04_run_full_scoring.R")   # full baseline V0
run("05_aggregate_occupation.R")      # composites + classification (+ primary_mode)
if (RUN_SCORING) run("11_calibration_experiment.R")  # sigma^2_g, sensitivities, flips

run("06_crosswalk.R")                 # O*NET -> UK SOC 2020
run("07_aps_employment.R")            # APS weights (+ counts, comparators, bootstrap)
run("08_region_indices_shiftshare.R") # continuous headline + channel mix + decomposition
run("16_comparators.R")               # Scotland / London / rUK-ex-London, GOR ranking
run("09_employment_regressions.R")    # employment regressions
run("13_css.R")                       # task-level classification stability
run("12_montecarlo.R")                # scoring / sampling / composed intervals
run("10_acemoglu_projection.R")       # productivity + GDP (both channels)

if (RUN_SCORING) run("14_common_mode_test.R")   # cross-model robustness (CM_ARM)
run("15_wage_eiv.R")                  # conditional on ASHE

message("\nPipeline complete. Tables in out/tables, figures in out/figures.")
