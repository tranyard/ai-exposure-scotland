# =====================================================================
# 02_prompt.R  —  Frozen scoring prompt + variant generators (Plan §2.1, §2.4).
# The baseline prompt is hashed (SHA-256) so the run is near-deterministic
# and the exact wording is auditable. Variants V0-V4 implement the
# calibration design; V0-V3 are within-model, V4 is cross-model.
# =====================================================================
source(here::here("R", "00_config.R"))

# --- System message (baseline V0) ------------------------------------
.system_template <- function(horizon = 10, reverse_criteria = FALSE,
                             single_score = FALSE) {
  year <- 2025 + horizon
  criteria <- c(
    "- Does the task primarily involve processing information, generating text, analysing data, or pattern recognition? (Increases both scores)",
    "- Does the task require physical dexterity or manipulation of objects in unstructured environments? (Decreases substitution)",
    "- Does it require human empathy, interpersonal trust, legal accountability, or ethical judgment in high-stakes situations? (Decreases substitution; may still increase complementarity)",
    "- Does it involve creative or strategic decisions in genuinely novel situations where there is no established pattern to learn from? (Decreases substitution)",
    "- Is the output of the task easily verified for quality? (Increases substitution)",
    "- Is the task highly routinised and rule-based? (Increases substitution)"
  )
  if (reverse_criteria) criteria <- rev(criteria)

  scoring_block <- if (single_score) {
    paste0(
      "Score this task on a single dimension:\n",
      "AI_EXPOSURE (0-100): The degree to which AI could substantially affect how this task is performed within the horizon, whether by full automation or by significant augmentation.\n"
    )
  } else {
    paste0(
      "You will score each task on two dimensions:\n",
      "SUBSTITUTION (0-100): The degree to which AI could fully replace a human worker performing this task end-to-end, with no meaningful human involvement. 0 = cannot substitute at all; 100 = near-certain full automation feasible within the horizon.\n",
      "COMPLEMENTARITY (0-100): The degree to which AI could significantly augment a human worker performing this task, increasing productivity without replacing them. 0 = no augmentation; 100 = very substantial augmentation while the human remains essential.\n",
      "Note: these two scores need not sum to 100.\n"
    )
  }

  json_block <- if (single_score) {
    '{"ai_exposure": <integer 0-100>, "primary_mode": "<substitution | complementarity | none>", "key_factors": "<one sentence, max 20 words>"}'
  } else {
    '{"substitution": <integer 0-100>, "complementarity": <integer 0-100>, "primary_mode": "<substitution | complementarity | none>", "key_factors": "<one sentence, max 20 words>"}'
  }

  paste0(
    "You are an expert economist and AI capability researcher specialising in the economics of automation and labour markets. ",
    "Your task is to evaluate the extent to which a specific occupational task -- as defined in the O*NET occupational database -- ",
    "will be affected by artificial intelligence systems within the next ", horizon,
    " years (i.e., by approximately ", year, ").\n\n",
    "When assessing AI capabilities, assume continued progress at approximately the pace observed between 2020 and 2024: ",
    "frontier large language models, multimodal AI systems, robotic automation, and AI-powered decision tools are all in scope. ",
    "Do not assume science-fiction-level capabilities beyond this trajectory.\n\n",
    scoring_block, "\n",
    "In your assessment, consider the following factors:\n",
    paste(criteria, collapse = "\n"), "\n\n",
    "Return ONLY the following JSON -- no preamble, no text outside the JSON:\n",
    json_block
  )
}

.user_template <- function(occupation_title, task_text) {
  paste0("Occupation: ", occupation_title, "\nTask: ", task_text,
         "\nScore this task for AI exposure within the horizon stated above.")
}

# --- Variant registry -------------------------------------------------
# Each variant returns the system message and a flag for parsing mode.
variant_spec <- function(variant, model = MODELS$M) {
  stopifnot(variant %in% CALIB$variants)
  switch(variant,
    V0 = list(system = .system_template(SCORING$horizon_years_baseline),
              single = FALSE, model = model),
    V1 = list(system = .system_template(SCORING$horizon_years_short),
              single = FALSE, model = model),
    V2 = list(system = .system_template(SCORING$horizon_years_baseline, reverse_criteria = TRUE),
              single = FALSE, model = model),
    V3 = list(system = .system_template(SCORING$horizon_years_baseline, single_score = TRUE),
              single = TRUE,  model = model),
    V4 = list(system = .system_template(SCORING$horizon_years_baseline),
              single = FALSE, model = MODELS$Mp)   # cross-model
  )
}

# --- Frozen baseline hash (record in the methods section) -------------
PROMPT_BASELINE_SYSTEM <- .system_template(SCORING$horizon_years_baseline)
PROMPT_HASH <- digest(PROMPT_BASELINE_SYSTEM, algo = "sha256")
message("baseline prompt SHA-256: ", PROMPT_HASH)
writeLines(c(PROMPT_HASH, "", PROMPT_BASELINE_SYSTEM),
           file.path(PATHS$cache, "prompt_baseline.txt"))
