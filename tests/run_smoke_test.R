# =====================================================================
# tests/run_smoke_test.R  —  End-to-end dry run, no network, no real data.
# Run from the project root:   Rscript tests/run_smoke_test.R
#
# What it does:
#   1. Creates an isolated sandbox (SMOKE_ROOT) so your real data/ and
#      out/ are never touched.
#   2. Generates synthetic, internally consistent inputs.
#   3. Runs the FULL pipeline with MOCK_SCORING=1 and RUN_SCORING=1, so
#      the scoring scripts (04, 11, 14) execute their real logic offline.
#   4. Asserts every expected output exists and key numbers are finite.
# Exits non-zero if any check fails.
# =====================================================================
stopifnot("run from the project root (Rscript tests/run_smoke_test.R)" =
            dir.exists("R") && file.exists(file.path("R", "_run_all.R")))

sandbox <- file.path(tempdir(), paste0("smoke_", as.integer(Sys.time())))
dir.create(sandbox, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(SMOKE_ROOT = sandbox, MOCK_SCORING = "1", RUN_SCORING = "1")
message("sandbox: ", sandbox)

t0 <- Sys.time()
ok <- tryCatch({
  source(file.path("tests", "00_make_synthetic_data.R"))
  source(file.path("R", "_run_all.R"))
  TRUE
}, error = function(e) { message("\nPIPELINE ERROR: ", conditionMessage(e)); FALSE })

# --- Assertions -------------------------------------------------------
expected <- c(
  "out/cache/task_df.csv", "out/cache/prompt_baseline.txt",
  "out/scores/task_scores_V0.csv", "out/cache/occupation_scores_V0.csv",
  "out/cache/sigma2_g.csv", "out/cache/cross_model_bias_calib.csv",
  "out/cache/uk_soc3_scores.csv", "out/cache/aps_pool.csv",
  "out/cache/region_panel.csv",
  "tables/region_indices.csv", "tables/shiftshare.csv",
  "tables/sector_exposure.csv", "tables/employment_regressions.txt",
  "tables/classification_stability.csv", "tables/montecarlo_intervals.csv",
  "tables/acemoglu_projection.csv", "tables/acemoglu_differential.csv",
  "tables/common_mode_test.csv", "tables/wage_eiv.csv"
)
present <- file.exists(file.path(sandbox, expected))
results <- tibble::tibble(file = expected, ok = present)
cat("\n================ SMOKE TEST: FILE CHECKS ================\n")
for (i in seq_along(expected))
  cat(sprintf("  [%s] %s\n", if (present[i]) "PASS" else "FAIL", expected[i]))

# --- Sanity checks on headline numbers -------------------------------
num_ok <- TRUE
chk <- function(label, cond) {
  cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "PASS" else "FAIL", label))
  if (!isTRUE(cond)) num_ok <<- FALSE
}
cat("\n================ SMOKE TEST: VALUE CHECKS ===============\n")
try({
  rd <- function(f) readr::read_csv(file.path(sandbox, f), show_col_types = FALSE)
  ri  <- rd("tables/region_indices.csv")
  ss  <- rd("tables/shiftshare.csv")
  mc  <- rd("tables/montecarlo_intervals.csv")
  cm  <- rd("tables/common_mode_test.csv")
  chk("region indices finite for both regions",
      nrow(ri) == 2 && all(is.finite(ri$E_hat)))
  chk("shift-share within-effect ~ 0 (by construction)",
      abs(ss$within) < 1e-8)
  chk("Monte Carlo Delta_E interval is finite",
      all(is.finite(unlist(mc[mc$quantity == "Delta_E", c("p2.5", "p97.5")]))))
  chk("common-mode: gap shift <= level shift (Prop 1 direction)",
      abs(cm$cross_model_shift[cm$quantity == "Delta_E_bar"]) <=
        max(abs(cm$cross_model_shift[cm$quantity != "Delta_E_bar"])) + 1e-9)
}, silent = FALSE)

# --- Verdict ----------------------------------------------------------
all_files <- all(present)
verdict <- ok && all_files && num_ok
cat("\n========================================================\n")
cat(sprintf("  pipeline ran : %s\n", if (ok) "yes" else "no"))
cat(sprintf("  files present: %d / %d\n", sum(present), length(present)))
cat(sprintf("  value checks : %s\n", if (num_ok) "all passed" else "some FAILED"))
cat(sprintf("  elapsed      : %.1fs\n", as.numeric(Sys.time() - t0, units = "secs")))
cat(sprintf("\n  OVERALL: %s\n", if (verdict) "PASS ✅" else "FAIL ❌"))
cat("========================================================\n")
cat("sandbox kept for inspection:\n  ", sandbox, "\n")

quit(save = "no", status = if (verdict) 0 else 1)
