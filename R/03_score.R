# =====================================================================
# 03_score.R  —  LLM scoring engine (Plan §2.1, §2.4).
# Provider-agnostic interface over httr2. Two transports:
#   * synchronous  -> small jobs (calibration, cross-model spot runs)
#   * Anthropic Batch API -> the full ~19,500-task run at 50% cost
# Output schema (flat CSV), identical regardless of transport/provider:
#   onet_soc_code, task_id, variant, provider, model, sub, comp,
#   primary_mode, key_factors, prompt_hash, run_date
#
# NOTE ON THE R<->PYTHON BOUNDARY: this reproduces, in R via httr2, the
# same job your Python Anthropic-SDK Batch script does. Either may be
# used; both emit this CSV schema, so the downstream pipeline is agnostic.
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

# --- Offline mock scorer (MOCK_SCORING=1) ----------------------------
# Returns deterministic synthetic scores with realistic structure: an
# occupation latent, task jitter, within-model variant noise (so V0-V3
# differ -> positive sigma^2_g), and a V4 level shift (-> a detectable
# cross-model bias for the common-mode test). No network is used.
MOCK <- nzchar(Sys.getenv("MOCK_SCORING"))
.unit_hash <- function(x) (strtoi(substr(digest(x, algo = "md5"), 1, 6), 16L) %% 100000L) / 100000
.mock_score_tasks <- function(tasks, variant) {
  spec    <- variant_spec(variant)
  v_delta <- c(V0 = 0, V1 = -4, V2 = 0, V3 = 0, V4 = 8)[[variant]]
  occ_lat <- 30 + 50 * vapply(tasks$onet_soc_code, .unit_hash, numeric(1))
  jit     <- 12 * (vapply(paste(tasks$onet_soc_code, tasks$task_id), .unit_hash, numeric(1)) - 0.5)
  vnoise  <- 8  * (vapply(paste(tasks$onet_soc_code, tasks$task_id, variant), .unit_hash, numeric(1)) - 0.5)
  sub  <- pmin(pmax(round(occ_lat + jit + v_delta + vnoise), 0), 100)
  comp <- pmin(pmax(round(0.85 * occ_lat + jit + vnoise + 5), 0), 100)
  if (identical(variant, "V3")) comp <- sub
  tibble(onet_soc_code = tasks$onet_soc_code, task_id = tasks$task_id,
         variant = variant, provider = spec$model$provider, model = spec$model$id,
         sub = sub, comp = comp,
         primary_mode = if_else(sub >= comp, "substitution", "complementarity"),
         key_factors = "mock", prompt_hash = PROMPT_HASH,
         run_date = as.character(Sys.Date()))
}

# --- Parse the model's JSON reply into (sub, comp, ...) ---------------
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

# --- One synchronous call --------------------------------------------
.score_one <- function(occupation_title, task_text, spec) {
  prov  <- spec$model$provider
  user  <- .user_template(occupation_title, task_text)
  body  <- if (prov == "anthropic") {
    list(model = spec$model$id, max_tokens = SCORING$max_tokens,
         temperature = SCORING$temperature, system = spec$system,
         messages = list(list(role = "user", content = user)))
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
  txt <- if (prov == "anthropic") resp_body_json(resp)$content[[1]]$text
         else                     resp_body_json(resp)$choices[[1]]$message$content
  .parse_score(txt %||% "", spec$single)
}

# --- Synchronous scoring of a task tibble under one variant ----------
score_sync <- function(tasks, variant) {
  if (MOCK) return(.mock_score_tasks(tasks, variant))
  spec <- variant_spec(variant)
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

# --- Anthropic Batch API (full run, 50% cheaper) ---------------------
# Submit -> poll -> retrieve. custom_id is a synthetic key joined back
# afterwards (SOC codes contain '.' which custom_id disallows).
batch_submit <- function(tasks, variant) {
  spec <- variant_spec(variant)
  if (MOCK) {
    keyed <- tasks |> mutate(custom_id = sprintf("r%07d", row_number()))
    saveRDS(keyed |> select(custom_id, onet_soc_code, task_id),
            file.path(PATHS$cache, paste0("batch_key_", variant, ".rds")))
    message("MOCK batch submitted: ", variant, " (", nrow(tasks), " tasks)")
    return(paste0("mock_", variant))
  }
  if (spec$model$provider != "anthropic")
    stop("Batch transport is Anthropic-only; use score_sync() for ", spec$model$id, ".")
  keyed <- tasks |> mutate(custom_id = sprintf("r%07d", row_number()))
  requests <- pmap(list(keyed$custom_id, keyed$occupation_title, keyed$task),
    function(cid, ot, tx) list(
      custom_id = cid,
      params = list(model = spec$model$id, max_tokens = SCORING$max_tokens,
                    temperature = SCORING$temperature, system = spec$system,
                    messages = list(list(role = "user", content = .user_template(ot, tx))))
    ))
  resp <- request("https://api.anthropic.com/v1/messages/batches") |>
    req_headers(`x-api-key` = .api_key("anthropic"),
                `anthropic-version` = SCORING$anthropic_version,
                `content-type` = "application/json") |>
    req_body_json(list(requests = requests)) |>
    req_perform() |> resp_body_json()
  saveRDS(keyed |> select(custom_id, onet_soc_code, task_id),
          file.path(PATHS$cache, paste0("batch_key_", variant, ".rds")))
  message("batch submitted: ", resp$id, " (", length(requests), " requests)")
  resp$id
}

batch_collect <- function(batch_id, variant, poll_seconds = 60) {
  if (MOCK) {
    key <- readRDS(file.path(PATHS$cache, paste0("batch_key_", variant, ".rds")))
    return(.mock_score_tasks(key, variant))
  }
  hdr <- c(`x-api-key` = .api_key("anthropic"),
           `anthropic-version` = SCORING$anthropic_version)
  repeat {
    st <- request(paste0("https://api.anthropic.com/v1/messages/batches/", batch_id)) |>
      req_headers(!!!hdr) |> req_perform() |> resp_body_json()
    message(Sys.time(), "  status=", st$processing_status,
            "  done=", st$request_counts$succeeded, "/", sum(unlist(st$request_counts)))
    if (st$processing_status == "ended") break
    Sys.sleep(poll_seconds)
  }
  spec <- variant_spec(variant)
  lines <- request(st$results_url) |> req_headers(!!!hdr) |>
    req_perform() |> resp_body_string() |> str_split_1("\n") |> discard(~ .x == "")
  parsed <- map_dfr(lines, function(ln) {
    o <- fromJSON(ln, simplifyVector = FALSE)
    txt <- if (identical(o$result$type, "succeeded"))
      o$result$message$content[[1]]$text else ""
    ps <- .parse_score(txt, spec$single)
    tibble(custom_id = o$custom_id, sub = ps$sub, comp = ps$comp,
           primary_mode = ps$primary_mode, key_factors = ps$key_factors)
  })
  key <- readRDS(file.path(PATHS$cache, paste0("batch_key_", variant, ".rds")))
  key |> left_join(parsed, by = "custom_id") |>
    transmute(onet_soc_code, task_id, variant = variant, provider = "anthropic",
              model = spec$model$id, sub, comp, primary_mode, key_factors,
              prompt_hash = PROMPT_HASH, run_date = as.character(Sys.Date()))
}
