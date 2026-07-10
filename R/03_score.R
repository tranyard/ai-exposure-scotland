# =====================================================================
# 03_score.R  —  LLM scoring engine
# Provider-agnostic interface over httr2. Two transports:
#   * synchronous          -> small jobs and NA retries
#   * Anthropic Batch API  -> the full runs and the calibration variants
#     (50% cheaper; a batch holds up to 100,000 requests, so the ~19,500
#     task set fits in one submission)
#
# Resumability: batch_run() persists the batch ID to out/cache the moment
# a batch is accepted. If the session dies mid-poll, re-running the same
# call resumes collection from the stored ID instead of resubmitting
# (and re-paying for) the batch. Results remain retrievable server-side
# for 29 days.
#
# Note on prompt caching: the system message (~450 tokens) is below the
# 1,024-token minimum cacheable prefix, so cache_control would be a
# no-op here. Revisit only if the rubric grows past that threshold.
#
# Output schema:
#   onet_soc_code, task_id, variant, provider, model, sub, comp,
#   primary_mode, key_factors, prompt_hash, run_date
# =====================================================================
source(here::here("R", "02_prompt.R"))
suppressPackageStartupMessages({
  library(httr2); library(jsonlite); library(furrr); library(future)
})

.api_key <- function(provider) {
  v <- switch(provider, anthropic = Sys.getenv("ANTHROPIC_API_KEY"),
              openai    = Sys.getenv("OPENAI_API_KEY"), "")
  if (!nzchar(v)) stop("Set the API key env var for provider '", provider, "'.", call. = FALSE)
  v
}

# --- Offline mock scorer (MOCK_SCORING=1) for testing ------------------
# Returns deterministic synthetic scores with realistic structure: an
# occupation latent, task jitter, within-model variant noise (so the
# noise variants differ -> positive sigma^2_g), and a cross-model level
# shift when a non-baseline model is passed. No network is used.
# NB: this flag definition must survive any tidying of the mock block --
# every transport below branches on MOCK.
MOCK <- nzchar(Sys.getenv("MOCK_SCORING"))
.unit_hash <- function(x) (strtoi(substr(digest(x, algo = "md5"), 1, 6), 16L) %% 100000L) / 100000
.mock_score_tasks <- function(tasks, variant, model = MODELS$M) {
  spec    <- variant_spec(variant, model)
  v_delta <- c(V0 = 0, V1 = -6, V2 = 0.5, V3 = 2, V5 = -0.5, V6 = 0.5)[[variant]]
  m_delta <- if (identical(model$id, MODELS$M$id)) 0 else 8   # cross-model shift
  occ_lat <- 30 + 50 * vapply(tasks$onet_soc_code, .unit_hash, numeric(1))
  jit     <- 12 * (vapply(paste(tasks$onet_soc_code, tasks$task_id), .unit_hash, numeric(1)) - 0.5)
  vnoise  <- 6  * (vapply(paste(tasks$onet_soc_code, tasks$task_id, variant, model$id),
                          .unit_hash, numeric(1)) - 0.5)
  sub  <- pmin(pmax(round(occ_lat + jit + v_delta + m_delta + vnoise), 0), 100)
  comp <- pmin(pmax(round(0.85 * occ_lat + jit + m_delta + vnoise + 5), 0), 100)
  if (identical(variant, "V3")) comp <- sub
  tibble(onet_soc_code = tasks$onet_soc_code, task_id = tasks$task_id,
         variant = variant, provider = spec$model$provider, model = spec$model$id,
         sub = sub, comp = comp,
         primary_mode = if_else(sub >= comp, "substitution", "complementarity"),
         key_factors = "mock", prompt_hash = PROMPT_HASH,
         run_date = as.character(Sys.Date()))
}

# --- Parse the model's JSON reply into (sub, comp, ...) ----------------
.parse_score <- function(text, single) {
  out <- list(sub = NA_real_, comp = NA_real_, primary_mode = NA_character_,
              key_factors = NA_character_)
  j <- tryCatch(fromJSON(str_extract(text, "\\{(?s).*\\}")), error = function(e) NULL)
  if (is.null(j)) return(out)
  if (single) {
    e <- as.numeric(j$ai_exposure %||% NA)
    out$sub <- e; out$comp <- e          # single-score variant: one number both channels
  } else {
    out$sub  <- as.numeric(j$substitution   %||% NA)
    out$comp <- as.numeric(j$complementarity %||% NA)
  }
  out$primary_mode <- as.character(j$primary_mode %||% NA)
  out$key_factors  <- as.character(j$key_factors  %||% NA)
  out
}

# --- Request bodies -----------------------------------------------------
# Sonnet 4.6 and later reject a trailing assistant turn ("does not support
# assistant message prefill"), so the request ends with the user message
# and the model returns the whole JSON object itself. .parse_score()
# isolates the {...} from any surrounding prose, so an incidental preamble
# or code fence is tolerated. OpenAI enforces JSON via response_format.
.anthropic_params <- function(spec, user) {
  list(model = spec$model$id, max_tokens = SCORING$max_tokens,
       temperature = SCORING$temperature, system = spec$system,
       messages = list(list(role = "user", content = user)))
}

# --- One synchronous call ----------------------------------------------
.score_one <- function(occupation_title, task_text, spec) {
  prov  <- spec$model$provider
  user  <- .user_template(occupation_title, task_text, reorder = spec$reorder)
  body  <- if (prov == "anthropic") {
    .anthropic_params(spec, user)
  } else {
    list(model = spec$model$id, temperature = SCORING$temperature,
         max_tokens = SCORING$max_tokens, response_format = list(type = "json_object"),
         messages = list(list(role = "system", content = spec$system),
                         list(role = "user",   content = user)))
  }
  url <- if (prov == "anthropic") "https://api.anthropic.com/v1/messages"
  else                     "https://api.openai.com/v1/chat/completions"
  req <- request(url) |> req_body_json(body) |> req_retry(max_tries = SCORING$retries + 1)
  req <- if (prov == "anthropic")
    req_headers(req, `x-api-key` = .api_key(prov),
                `anthropic-version` = SCORING$anthropic_version,
                `content-type` = "application/json")
  else
    req_auth_bearer_token(req, .api_key(prov))

  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp)) return(.parse_score("", spec$single))
  txt <- if (prov == "anthropic")
    resp_body_json(resp)$content[[1]]$text %||% ""
  else
    resp_body_json(resp)$choices[[1]]$message$content %||% ""
  .parse_score(txt, spec$single)
}

# --- Synchronous scoring of a task tibble under one variant ------------
score_sync <- function(tasks, variant, model = MODELS$M) {
  if (MOCK) return(.mock_score_tasks(tasks, variant, model))
  spec <- variant_spec(variant, model)
  plan(multisession, workers = SCORING$concurrency); on.exit(plan(sequential), add = TRUE)
  res <- future_pmap(
    list(tasks$occupation_title, tasks$task),
    function(ot, tx) .score_one(ot, tx, spec),
    .options = furrr_options(seed = TRUE)
  )
  bind_cols(
    tasks |> select(onet_soc_code, task_id),
    tibble(variant = variant, provider = spec$model$provider, model = spec$model$id,
           sub = map_dbl(res, "sub"), comp = map_dbl(res, "comp"),
           primary_mode = map_chr(res, "primary_mode"),
           key_factors = map_chr(res, "key_factors"),
           prompt_hash = PROMPT_HASH, run_date = as.character(Sys.Date()))
  )
}

# --- Synchronous scoring with checkpointing ----------------------------
# For long sync jobs (the OpenAI full re-score in script 14). Scores in
# chunks, appending each chunk to a checkpoint CSV, and skips rows that
# are already scored, so an interrupted run resumes for free.
score_sync_chunked <- function(tasks, variant, tag, model = MODELS$M,
                               chunk = 500L) {
  ckpt <- file.path(PATHS$scores, paste0("task_scores_", tag, ".csv"))
  done <- if (file.exists(ckpt)) read_csv(ckpt, show_col_types = FALSE) else NULL
  todo <- if (is.null(done)) tasks else
    tasks |> anti_join(done, by = c("onet_soc_code", "task_id"))
  message(tag, ": ", nrow(tasks) - nrow(todo), " already scored, ",
          nrow(todo), " to go.")
  while (nrow(todo) > 0) {
    batch <- todo |> slice_head(n = chunk)
    s <- score_sync(batch, variant, model)
    write_csv(s, ckpt, append = file.exists(ckpt))
    todo <- todo |> slice_tail(n = max(nrow(todo) - chunk, 0))
    message(tag, ": ", nrow(todo), " remaining ...")
  }
  read_scores(ckpt)
}

# --- Anthropic Batch API (50% cheaper; asynchronous) --------------------
.batch_hdr <- function() c(`x-api-key` = .api_key("anthropic"),
                           `anthropic-version` = SCORING$anthropic_version)

batch_submit <- function(tasks, variant, tag, model = MODELS$M) {
  spec <- variant_spec(variant, model)
  keyed <- tasks |> mutate(custom_id = sprintf("r%07d", row_number()))
  saveRDS(keyed |> select(custom_id, onet_soc_code, task_id),
          file.path(PATHS$cache, paste0("batch_key_", tag, ".rds")))
  if (MOCK) {
    message("MOCK batch submitted: ", tag, " (", nrow(tasks), " tasks)")
    return(paste0("mock_", tag))
  }
  if (spec$model$provider != "anthropic")
    stop("Batch transport is Anthropic-only; use score_sync_chunked() for ",
         spec$model$id, ".")
  requests <- pmap(list(keyed$custom_id, keyed$occupation_title, keyed$task),
                   function(cid, ot, tx) list(
                     custom_id = cid,
                     params = .anthropic_params(spec, .user_template(ot, tx, reorder = spec$reorder))
                   ))
  resp <- request("https://api.anthropic.com/v1/messages/batches") |>
    req_headers(!!!.batch_hdr(), `content-type` = "application/json") |>
    req_body_json(list(requests = requests)) |>
    req_perform() |> resp_body_json()
  message("batch submitted: ", resp$id, " (", length(requests), " requests)")
  resp$id
}

batch_collect <- function(batch_id, variant, tag, model = MODELS$M,
                          poll_seconds = 60) {
  spec <- variant_spec(variant, model)
  key  <- readRDS(file.path(PATHS$cache, paste0("batch_key_", tag, ".rds")))
  if (MOCK) return(.mock_score_tasks(
    key |> left_join(read_csv(file.path(PATHS$cache, "task_df.csv"),
                              show_col_types = FALSE),
                     by = c("onet_soc_code", "task_id")),
    variant, model))
  repeat {
    st <- request(paste0("https://api.anthropic.com/v1/messages/batches/", batch_id)) |>
      req_headers(!!!.batch_hdr()) |> req_perform() |> resp_body_json()
    message(Sys.time(), "  status=", st$processing_status,
            "  done=", st$request_counts$succeeded, "/", sum(unlist(st$request_counts)))
    if (st$processing_status == "ended") break
    Sys.sleep(poll_seconds)
  }
  lines <- request(st$results_url) |> req_headers(!!!.batch_hdr()) |>
    req_perform() |> resp_body_string() |> str_split_1("\n") |> discard(~ .x == "")
  parsed <- map_dfr(lines, function(ln) {
    o <- fromJSON(ln, simplifyVector = FALSE)
    block <- if (identical(o$result$type, "succeeded"))
      tryCatch(o$result$message$content[[1]]$text, error = function(e) NULL)
    txt <- block %||% ""
    ps <- .parse_score(txt, spec$single)
    tibble(custom_id = o$custom_id, sub = ps$sub, comp = ps$comp,
           primary_mode = ps$primary_mode, key_factors = ps$key_factors)
  })
  key |> left_join(parsed, by = "custom_id") |>
    transmute(onet_soc_code, task_id, variant = variant,
              provider = spec$model$provider, model = spec$model$id,
              sub, comp, primary_mode, key_factors,
              prompt_hash = PROMPT_HASH, run_date = as.character(Sys.Date()))
}

# --- Resumable batch wrapper -------------------------------------------
# The single entry point every batch consumer uses. State machine:
#   scores CSV exists          -> read and return (nothing to pay for)
#   batch ID file exists       -> resume polling that batch
#   otherwise                  -> submit, persist the ID, then poll
# Delete BOTH the scores CSV and the ID file to force a fresh run.
batch_run <- function(tasks, variant, tag, model = MODELS$M,
                      poll_seconds = 60) {
  out_f <- file.path(PATHS$scores, paste0("task_scores_", tag, ".csv"))
  id_f  <- file.path(PATHS$cache,  paste0("batch_id_", tag, ".txt"))
  if (file.exists(out_f)) {
    message("already scored: ", out_f)
    return(read_scores(out_f))
  }
  bid <- if (file.exists(id_f)) {
    message("resuming batch from stored ID: ", readLines(id_f)[1])
    readLines(id_f)[1]
  } else {
    b <- batch_submit(tasks, variant, tag, model)
    writeLines(b, id_f)             # persist BEFORE polling: crash-safe
    b
  }
  scores <- batch_collect(bid, variant, tag, model, poll_seconds)
  save_csv(scores, out_f)
  scores
}
