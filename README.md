# Scotland vs rUK AI-Exposure Decomposition

MSc Economics dissertation in collaboration with the Scottish Fiscal Commission.
R pipeline implementing the revised plan: the locked empirical core (APS employment
regressions, shift-share, Acemoglu calibration) plus the updated exposure measurement
treatment (within-model classical error, calibration, Monte Carlo propagation,
common-mode cancellation test, classification stability).

## Project structure

```
ai-exposure-scotland/
├── ai-exposure-scotland.Rproj    ← open this in RStudio
├── .here                         ← here() anchor
├── .gitignore
├── README.md
│
├── R/                            ← pipeline scripts (numbered in run order)
│   ├── 00_config.R               central paths, model pins, thresholds, seed
│   ├── 01_prepare_onet.R         build ~19,500 task–occupation pairs
│   ├── 02_prompt.R               frozen prompt + SHA-256 hash + V0–V4 variants
│   ├── 03_score.R                scoring engine: Anthropic Batch API + OpenAI sync
│   ├── 04_run_full_scoring.R     full baseline scoring → out/scores/
│   ├── 05_aggregate_occupation.R importance-weighted E_j, classification
│   ├── 06_crosswalk.R            O*NET-SOC → UK SOC 2020 (direct, 3-digit)
│   ├── 07_aps_employment.R       pooled APS shares + year panel
│   ├── 08_region_indices_shiftshare.R  region indices, gap, shift-share
│   ├── 09_employment_regressions.R     occupation + industry regressions (fixest)
│   ├── 10_acemoglu_projection.R  TFP/GDP projection + Scotland–rUK band
│   ├── 11_calibration_experiment.R     V0–V4 calibration, sigma^2_g, cross-model bias
│   ├── 12_montecarlo.R           R=1000 propagation → credible intervals
│   ├── 13_css.R                  classification stability scores
│   ├── 14_common_mode_test.R     re-score on M'; Prop 1 + Cor 1 tests
│   ├── 15_wage_eiv.R             conditional wage regression + EIV correction
│   ├── _run_all.R                orchestrator (runs everything in order)
│   └── install_packages.R        one-off dependency install
│
├── tests/                        ← smoke test (no API, no real data)
│   ├── 00_make_synthetic_data.R  generates fake inputs
│   └── run_smoke_test.R          runs full pipeline offline with assertions
│
├── paper/                        ← LaTeX source
│   ├── main-2.tex                your dissertation (copy in from old repo)
│   ├── references.bib            Zotero auto-export target
│   └── revised_plan_and_measurement.*
│
├── figures/                      ← R writes here; LaTeX reads ../figures
├── tables/                       ← R writes here; LaTeX reads ../tables
│
├── data/                         ← YOUR INPUT DATA (gitignored — see below)
│   ├── onet/                     Task Statements.xlsx, Task Ratings.xlsx, Occupation Data.xlsx
│   ├── crosswalks/               soc2020_to_onet.csv  (your "mapping SOC2020-ONET2019" sheet)
│   ├── aps/                      aps_employment_soc_region.csv [+ ashe_microdata.csv if available]
│   └── sfc/                      acemoglu_calibration.csv [+ optional: task_cost_share, gdp_passthrough, info_env]
│
└── out/                          ← pipeline intermediates (gitignored)
    ├── scores/                   raw LLM score CSVs
    └── cache/                    intermediate files passed between scripts
```

## Quick start

```bash
# 1. Install R dependencies (once)
Rscript R/install_packages.R

# 2. Smoke test (no API key, no data, no network)
Rscript tests/run_smoke_test.R

# 3. Analysis only (after scoring is done and CSVs are in out/)
Rscript R/_run_all.R

# 4. Full run including LLM scoring (needs API keys)
export ANTHROPIC_API_KEY=sk-ant-...
export OPENAI_API_KEY=sk-...
RUN_SCORING=1 Rscript R/_run_all.R
```

## What is and isn't committed to git

| Tracked | Gitignored |
|---|---|
| `R/`, `tests/`, `paper/` | `data/` (licensed microdata) |
| `figures/`, `tables/` (LaTeX outputs) | `out/` (raw scores, caches) |
| `.Rproj`, `.here`, `.gitignore` | `.Renviron`, `.env` (API keys) |

## Reproducibility

`temperature = 0`, pinned model snapshots in `00_config.R`, a single frozen
seed, and a SHA-256 hash of the baseline system prompt written to
`out/cache/prompt_baseline.txt`. Reproducibility is near-deterministic, not
bit-identical (GPU floating-point non-determinism remains).
