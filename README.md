# Scotland vs rUK AI-Exposure Decomposition

MSc Economics dissertation in collaboration with the Scottish Fiscal Commission.
R pipeline implementing the locked empirical core (APS employment regressions,
shift-share, Acemoglu calibration) plus the exposure measurement treatment
(within-model classical error, calibration, Monte Carlo propagation, common-mode
cancellation test, classification stability), and the robustness modules on
resolution, industry composition, comparator regions, and fiscal translation.

## Project structure

```
ai-exposure-scotland/
├── ai-exposure-scotland.Rproj    ← open this in RStudio
├── .here                         ← here() anchor
├── .gitignore
├── README.md
│
├── R/                            ← pipeline scripts (numbered in run order)
│   ├── 00_config.R               paths, model pins, thresholds, seed, table writers
│   ├── 01_prepare_onet.R         build ~19,500 task–occupation pairs
│   ├── 02_prompt.R               frozen prompt + SHA-256 hash + variants V0–V3, V5, V6
│   ├── 03_score.R                scoring engine: Anthropic Batch API + sync transport
│   ├── 03b_openai_batch.R        OpenAI Batch API transport (sourced by 14)
│   ├── 04_run_full_scoring.R     full baseline scoring → out/scores/
│   ├── 05_aggregate_occupation.R importance-weighted E_j, classification, primary mode
│   ├── 06_crosswalk.R            O*NET-SOC → UK SOC 2020, unit group then minor group
│   ├── 07_aps_employment.R       APS shares, counts, comparator + industry pools,
│   │                               person bootstrap, worker wage extract
│   ├── 08_region_indices_shiftshare.R  continuous headline gap, channel mix,
│   │                               per-occupation decomposition, shift-share
│   ├── 09_employment_regressions.R     occupation + industry regressions (fixest)
│   ├── 10_acemoglu_projection.R  TFP/GDP projection + Scotland–rUK band
│   ├── 10b_acemoglu_panelc.R     projection on the classified task-content share,
│   │                               at all three threshold pairs
│   ├── 11_calibration_experiment.R     calibration, sigma^2_g, estimand sensitivity
│   ├── 12_montecarlo.R           scoring / sampling / composed intervals (R = 1000)
│   ├── 13_css.R                  classification stability scores + cutoff sweep
│   ├── 14_common_mode_test.R     re-score on M′; Prop 1 + Cor 1; scale-free gaps
│   ├── 15_wage_eiv.R             conditional wage regression + EIV correction
│   ├── 16_comparators.R          Scotland / London / rUK-ex-London; GOR league table
│   ├── 17_resolution_robustness.R      SOC4 differential + crosswalk aggregation rules
│   ├── 18_industry_decomposition.R     industry mix vs within-industry split
│   ├── 19_industry_invariance.R        total-variation diagnostic on Assumption 1
│   ├── 20_elasticity.R           NSND revenue elasticity + fiscal translation
│   ├── _run_all.R                orchestrator (runs everything in order)
│   └── install_packages.R        one-off dependency install
│
├── tests/                        ← smoke test (no API, no real data)
│   ├── 00_make_synthetic_data.R  generates fake inputs
│   └── run_smoke_test.R          runs full pipeline offline with assertions
│
├── paper/                        ← LaTeX source - download this and compile 
│   ├── main.tex                  dissertation root
│   ├── references.bib            Zotero auto-export target
│   └── ...                       section and appendix files
│
├── figures/                      ← R writes here; LaTeX reads ../figures
├── tables/                       ← R writes .tex fragments; LaTeX \input{}s them
│
├── data/                         ← YOUR INPUT DATA (gitignored — see below)
│   ├── onet/                     Task Statements.xlsx, Task Ratings.xlsx, Occupation Data.xlsx
│   ├── crosswalks/               soc2020_to_onet.csv  (the "mapping SOC2020-ONET2019" sheet)
│   ├── aps/                      aps_<year>.csv, one person-level file per wave
│   │                               [+ ashe_microdata.csv if available]
│   └── sfc/                      acemoglu_calibration.csv
│                                   [+ optional: task_cost_share.csv, gdp_passthrough.csv]
│
└── out/                          ← pipeline intermediates (gitignored)
    ├── scores/                   raw LLM score CSVs
    └── cache/                    intermediate files passed between scripts
        └── tables_csv/           machine-readable mirrors of tables/*.tex
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

`_run_all.R` runs every numbered script. The three unnumbered entries — `00_config.R`,
`03_score.R`, `03b_openai_batch.R` — are function libraries pulled in by `source()`
rather than run directly. Without `RUN_SCORING`, stages 04, 11 and 14 are skipped and
everything else re-runs cheaply from the cached scores.

## APS input format

`07_aps_employment.R` reads **person-level** microdata, one CSV per wave, named
`aps_<year>.csv`. Column names are mapped in `APS$vars` (`00_config.R`); the required
five are the person weight, COUNTRY, ILODEFR, SC20MMN and REFWKY. Optional columns
(region, industry, hourly pay, income weight, age, sex, FTPT, qualification, public
sector) are reported at read time when absent, since `bind_rows` NA-fills a wave that
lacks one rather than erroring.

Two APS quirks are handled explicitly and are worth knowing about:

- **Scotland is COUNTRY ∈ {3, 4}.** Code 4 is Scotland north of the Caledonian Canal.
- **Region is `GOR9D` to 2024 and `GOR9DCENSUS2021` in 2025.** Both names are accepted
  and coalesced into one column; under a single fixed name the 2025 wave is NA-filled
  and silently dropped.

`17_resolution_robustness.R` Part A additionally needs a four-digit unit-group variable,
which the public EUL file does not carry. It skips cleanly on public data and runs
against a secure-access extract via `APS_SOC4_VAR`.

## Environment variables

| Variable | Effect |
|---|---|
| `ANTHROPIC_API_KEY`, `OPENAI_API_KEY` | required for any stage that scores |
| `RUN_SCORING` | set to include stages 04, 11, 14 |
| `MOCK_SCORING` | offline deterministic synthetic scores; no network |
| `SMOKE_ROOT` | relocate the project root; relaxes the Scotland-records check |
| `APS_BOOT_B` | person-bootstrap replicates (default 500) |
| `CM_ARM` | cross-model arm: `Mpp` (default, Haiku), `Mp` (GPT-4o), or `BOTH` |
| `CM_SUBSAMPLE` | re-score only the N highest-employment O*NET occupations |
| `APS_SOC4_VAR` | unit-group variable name for the SOC4 robustness (default `SOC20M`) |

## Outputs

Every `save_csv()` call targeting `tables/` writes a **booktabs `tabular` fragment** to
`tables/<name>.tex` for `\input{}` (requires `\usepackage{booktabs}`), plus a CSV mirror
in `out/cache/tables_csv/` so downstream scripts can read it back via `table_csv()`.
Writes to `out/` are plain CSV.

| Script | Paper tables |
|---|---|
| 08 | `region_indices_continuous`, `region_indices`, `channel_mix`, `pm_robustness`, `gap_decomposition`, `operator_robustness`, `shiftshare`, `threshold_sensitivity`, `sector_exposure` |
| 09 | `employment_regressions` |
| 10, 10b | `acemoglu_projection`, `acemoglu_differential`, `gdp_projection`, `acemoglu_panelc`, `acemoglu_panelc_differential` |
| 11, 12, 13 | `calib_sensitivity`, `montecarlo_intervals`, `classification_stability`, `stability_validation`, `stability_cutoff_sweep` |
| 14 | `common_mode_test`, `cross_model_standardised` |
| 15 | `wage_eiv`, `wage_specs_key` |
| 16 | `comparator_indices`, `comparator_gaps`, `comparator_decomposition`, `gor_ranking` |
| 17, 18, 19 | `soc4_differential`, `crosswalk_aggregation_sensitivity`, `industry_decomposition`, `industry_contributions`, `industry_contributions_full`, `industry_invariance_summary`, `industry_tv_by_occupation` |
| 20 | `nsnd_elasticity`, `fiscal_translation` |

`20_elasticity.R` also prints a block of `\renewcommand` macro definitions to stdout for
pasting into `preamble.tex`.

## Consistency checks

Several stages assert against the headline gap rather than recomputing it independently,
so a stale cache surfaces as a failure rather than a quietly different number:

- `12_montecarlo.R` — the unperturbed draw must reproduce the headline gap exactly
  (`stopifnot`, 1e-8).
- `17_resolution_robustness.R` — the baseline aggregation rule must reproduce the
  headline gap, else a drift warning.
- `18_industry_decomposition.R` — both weighting orderings must sum to the gap computed
  on the same sample, and the demeaned per-section terms must sum to the aggregate.
- `07_aps_employment.R` — employment shares must sum to one within region.

## What is and isn't committed to git

| Tracked | Gitignored |
|---|---|
| `R/`, `tests/`, `paper/` | `data/` (licensed microdata) |
| `figures/`, `tables/` (LaTeX outputs) | `out/` (raw scores, caches) |
| `.Rproj`, `.here`, `.gitignore` | `.Renviron`, `.env` (API keys) |

## Reproducibility

`temperature = 0`, pinned model snapshots in `00_config.R`, a single frozen seed, and a
SHA-256 hash of the baseline system prompt written to `out/cache/prompt_baseline.txt`.
The hash covers both the system message and the user-message template, so a change to
either is detectable. Batch runs are resumable: the batch ID is persisted the moment a
batch is accepted, so a dropped session resumes polling rather than resubmitting.
Reproducibility is near-deterministic, not bit-identical (GPU floating-point
non-determinism remains).
