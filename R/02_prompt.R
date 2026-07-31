# Frozen scoring prompt and variant generators. The hash covers both the
# system message and the user-message template, so a change to either is
# detectable.
# Variants:
#   V0  baseline (10-year horizon, two scores)          noise reference
#   V1  5-year horizon                                  estimand sensitivity
#   V2  criteria in reverse order                       noise: order/anchoring
#   V3  single composite score                          estimand sensitivity
#   V5  paraphrased rubric (same meaning)               noise: wording
#   V6  user message reordered (task before occupation) noise: framing
source(here::here("R", "00_config.R"))

.system_template <- function(horizon = SCORING$horizon_years_baseline,
                             reverse_criteria = FALSE,
                             single_score = FALSE) {
  year <- SCORING$base_year + horizon
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
    "When assessing AI capabilities, assume continued progress at approximately the pace observed between 2020 and 2025: ",
    "frontier large language models, multimodal AI systems, robotic automation, and AI-powered decision tools are all in scope. ",
    "Do not assume science-fiction-level capabilities beyond this trajectory.\n\n",
    scoring_block, "\n",
    "In your assessment, consider the following factors:\n",
    paste(criteria, collapse = "\n"), "\n\n",
    "Return ONLY the following JSON -- no preamble, no text outside the JSON:\n",
    json_block
  )
}

# V5: meaning-preserving rewording of the baseline. Identical horizon,
# dimensions, scale endpoints, criteria and JSON keys (parsing depends on
# them); only the phrasing changes, so V0-vs-V5 identifies wording
# sensitivity alone.
.system_paraphrase <- function(horizon = SCORING$horizon_years_baseline) {
  year <- SCORING$base_year + horizon
  criteria <- c(
    "- Is the core of the task information processing, text generation, data analysis, or pattern recognition? (Raises both scores)",
    "- Does carrying out the task depend on physical dexterity or on handling objects in unstructured settings? (Lowers substitution)",
    "- Does the task rest on human empathy, interpersonal trust, legal accountability, or ethical judgement where the stakes are high? (Lowers substitution; complementarity may still rise)",
    "- Does the task call for creative or strategic choices in genuinely novel situations with no established pattern to learn from? (Lowers substitution)",
    "- Can the quality of the task's output be checked easily? (Raises substitution)",
    "- Is the task strongly routinised and governed by rules? (Raises substitution)"
  )
  paste0(
    "You are an economist with expertise in AI capabilities, specialising in how automation reshapes labour markets. ",
    "Assess how far a specific occupational task, as defined in the O*NET occupational database, ",
    "will be affected by artificial intelligence systems over the coming ", horizon,
    " years (that is, by roughly ", year, ").\n\n",
    "In judging AI capability, assume progress continues at broadly the pace seen between 2020 and 2025: ",
    "frontier large language models, multimodal systems, robotic automation, and AI decision tools all count. ",
    "Do not assume capabilities beyond that trajectory.\n\n",
    "Rate the task on two dimensions:\n",
    "SUBSTITUTION (0-100): How far AI could carry out this task end-to-end in place of a human worker, with no meaningful human involvement. 0 = no substitution possible; 100 = full automation near-certain to be feasible within the horizon.\n",
    "COMPLEMENTARITY (0-100): How far AI could substantially raise the productivity of a human worker performing this task without replacing them. 0 = no augmentation; 100 = very large augmentation with the human remaining essential.\n",
    "The two scores need not sum to 100.\n\n",
    "Weigh the following considerations:\n",
    paste(criteria, collapse = "\n"), "\n\n",
    "Reply with ONLY this JSON -- no preamble and no text outside it:\n",
    '{"substitution": <integer 0-100>, "complementarity": <integer 0-100>, "primary_mode": "<substitution | complementarity | none>", "key_factors": "<one sentence, max 20 words>"}'
  )
}

.user_template <- function(occupation_title, task_text, reorder = FALSE) {
  if (reorder) {
    paste0("Task: ", task_text, "\nOccupation: ", occupation_title,
           "\nScore this task for AI exposure within the horizon stated above.")
  } else {
    paste0("Occupation: ", occupation_title, "\nTask: ", task_text,
           "\nScore this task for AI exposure within the horizon stated above.")
  }
}

variant_spec <- function(variant, model = MODELS$M) {
  stopifnot(variant %in% c("V0", "V1", "V2", "V3", "V5", "V6"))
  switch(variant,
    V0 = list(system = .system_template(SCORING$horizon_years_baseline),
              single = FALSE, reorder = FALSE, model = model),
    V1 = list(system = .system_template(SCORING$horizon_years_short),
              single = FALSE, reorder = FALSE, model = model),
    V2 = list(system = .system_template(SCORING$horizon_years_baseline,
                                        reverse_criteria = TRUE),
              single = FALSE, reorder = FALSE, model = model),
    V3 = list(system = .system_template(SCORING$horizon_years_baseline,
                                        single_score = TRUE),
              single = TRUE,  reorder = FALSE, model = model),
    V5 = list(system = .system_paraphrase(SCORING$horizon_years_baseline),
              single = FALSE, reorder = FALSE, model = model),
    V6 = list(system = .system_template(SCORING$horizon_years_baseline),
              single = FALSE, reorder = TRUE,  model = model)
  )
}

PROMPT_BASELINE_SYSTEM <- .system_template(SCORING$horizon_years_baseline)
PROMPT_BASELINE_USER   <- .user_template("<OCCUPATION>", "<TASK>")
PROMPT_HASH <- digest(paste(PROMPT_BASELINE_SYSTEM, PROMPT_BASELINE_USER,
                            sep = "\n---\n"), algo = "sha256")
message("baseline prompt SHA-256: ", PROMPT_HASH)
writeLines(c(PROMPT_HASH, "", PROMPT_BASELINE_SYSTEM, "",
             "--- user template ---", PROMPT_BASELINE_USER),
           file.path(PATHS$cache, "prompt_baseline.txt"))
