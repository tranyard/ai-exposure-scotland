# OpenAI Batch API transport. Source after 03_score.R, which defines
# .parse_score, .api_key, variant_spec, .user_template, PROMPT_HASH, MOCK
# and .mock_score_tasks. Output schema matches the Anthropic transport.

.openai_params <- function(spec, user) {
  list(model           = spec$model$id,
       temperature     = SCORING$temperature,
       max_tokens      = SCORING$max_tokens,
       response_format = list(type = "json_object"),
       messages = list(list(role = "system", content = spec$system),
                       list(role = "user",   content = user)))
}

.openai_hdr_key <- function() .api_key("openai")

.openai_batch_submit <- function(tasks, variant, tag, model = MODELS$Mp) {
  spec  <- variant_spec(variant, model)
  keyed <- tasks |> dplyr::mutate(custom_id = sprintf("r%07d", dplyr::row_number()))
  saveRDS(keyed |> dplyr::select(custom_id, onet_soc_code, task_id),
          file.path(PATHS$cache, paste0("batch_key_", tag, ".rds")))

  if (spec$model$provider != "openai")
    stop("openai_batch_run() is OpenAI-only; got ", spec$model$id, call. = FALSE)

  req_lines <- purrr::pmap_chr(
    list(keyed$custom_id, keyed$occupation_title, keyed$task),
    function(cid, ot, tx) {
      body <- .openai_params(spec, .user_template(ot, tx, reorder = spec$reorder))
      jsonlite::toJSON(
        list(custom_id = cid, method = "POST",
             url = "/v1/chat/completions", body = body),
        auto_unbox = TRUE)
    })
  jsonl <- file.path(PATHS$cache, paste0("batch_input_", tag, ".jsonl"))
  writeLines(req_lines, jsonl)

  up <- httr2::request("https://api.openai.com/v1/files") |>
    httr2::req_auth_bearer_token(.openai_hdr_key()) |>
    httr2::req_body_multipart(purpose = "batch",
                              file = curl::form_file(jsonl)) |>
    httr2::req_retry(max_tries = SCORING$retries + 1) |>
    httr2::req_perform() |> httr2::resp_body_json()

  cr <- httr2::request("https://api.openai.com/v1/batches") |>
    httr2::req_auth_bearer_token(.openai_hdr_key()) |>
    httr2::req_body_json(list(input_file_id     = up$id,
                              endpoint          = "/v1/chat/completions",
                              completion_window = "24h")) |>
    httr2::req_retry(max_tries = SCORING$retries + 1) |>
    httr2::req_perform() |> httr2::resp_body_json()
  message("batch created: ", cr$id, "  status=", cr$status)
  cr$id
}

.openai_batch_collect <- function(batch_id, variant, tag, model = MODELS$Mp,
                                  poll_seconds = 60) {
  spec <- variant_spec(variant, model)
  key  <- readRDS(file.path(PATHS$cache, paste0("batch_key_", tag, ".rds")))
  terminal <- c("completed", "failed", "expired", "cancelled")

  repeat {
    st <- httr2::request(paste0("https://api.openai.com/v1/batches/", batch_id)) |>
      httr2::req_auth_bearer_token(.openai_hdr_key()) |>
      httr2::req_perform() |> httr2::resp_body_json()
    rc <- st$request_counts
    message(Sys.time(), "  status=", st$status,
            "  done=", rc$completed %||% 0, "/", rc$total %||% 0,
            if ((rc$failed %||% 0) > 0) paste0("  failed=", rc$failed) else "")
    if (st$status %in% terminal) break
    Sys.sleep(poll_seconds)
  }

  if (identical(st$status, "failed")) {
    reason <- tryCatch(
      paste(vapply(st$errors$data, function(e) e$message %||% "", character(1)),
            collapse = " | "),
      error = function(e) "")
    stop("OpenAI batch ", batch_id, " failed at validation. Reason: ",
         if (nzchar(reason)) reason else "see platform.openai.com/batches.",
         call. = FALSE)
  }
  if ((st$error_file_id %||% "") != "")
    message("some requests errored; error file = ", st$error_file_id,
            " (missing rows carry NA and are handled downstream).")
  if ((st$output_file_id %||% "") == "")
    stop("batch ", batch_id, " ended '", st$status,
         "' with no output file. Inspect at platform.openai.com/batches.",
         call. = FALSE)

  lines <- httr2::request(paste0("https://api.openai.com/v1/files/",
                                 st$output_file_id, "/content")) |>
    httr2::req_auth_bearer_token(.openai_hdr_key()) |>
    httr2::req_perform() |> httr2::resp_body_string() |>
    stringr::str_split_1("\n") |> purrr::discard(~ .x == "")

  parsed <- purrr::map_dfr(lines, function(ln) {
    o   <- jsonlite::fromJSON(ln, simplifyVector = FALSE)
    txt <- tryCatch(o$response$body$choices[[1]]$message$content,
                    error = function(e) NULL) %||% ""
    ps  <- .parse_score(txt, spec$single)
    tibble::tibble(custom_id = o$custom_id, sub = ps$sub, comp = ps$comp,
                   primary_mode = ps$primary_mode, key_factors = ps$key_factors)
  })

  key |> dplyr::left_join(parsed, by = "custom_id") |>
    dplyr::transmute(onet_soc_code, task_id, variant = variant,
                     provider = spec$model$provider, model = spec$model$id,
                     sub, comp, primary_mode, key_factors,
                     prompt_hash = PROMPT_HASH, run_date = as.character(Sys.Date()))
}

.openai_batch_run_one <- function(tasks, variant, tag, model = MODELS$Mp,
                                  poll_seconds = 60) {
  out_f <- file.path(PATHS$scores, paste0("task_scores_", tag, ".csv"))
  id_f  <- file.path(PATHS$cache,  paste0("batch_id_",   tag, ".txt"))
  if (file.exists(out_f)) return(read_scores(out_f))

  bid <- if (file.exists(id_f)) {
    readLines(id_f)[1]
  } else {
    b <- .openai_batch_submit(tasks, variant, tag, model)
    writeLines(b, id_f)             # persist before polling: crash-safe
    b
  }
  scores <- .openai_batch_collect(bid, variant, tag, model, poll_seconds)
  save_csv(scores, out_f)
  scores
}

openai_batch_run <- function(tasks, variant, tag, model = MODELS$Mp,
                             poll_seconds = 60, max_per_batch = 2000L) {
  out_f <- file.path(PATHS$scores, paste0("task_scores_", tag, ".csv"))
  if (file.exists(out_f)) return(read_scores(out_f))
  if (MOCK) { s <- .mock_score_tasks(tasks, variant, model); save_csv(s, out_f); return(s) }

  if (nrow(tasks) <= max_per_batch)
    return(.openai_batch_run_one(tasks, variant, tag, model, poll_seconds))

  idx <- split(seq_len(nrow(tasks)),
               ceiling(seq_len(nrow(tasks)) / max_per_batch))
  parts <- purrr::imap(idx, function(rows, i) {
    ctag <- sprintf("%s_c%03d", tag, as.integer(i))
    message("chunk ", i, "/", length(idx), "  tag=", ctag,
            "  (", length(rows), " requests)")
    .openai_batch_run_one(tasks[rows, , drop = FALSE], variant, ctag,
                          model, poll_seconds)
  })
  scores <- dplyr::bind_rows(parts)
  save_csv(scores, out_f)
  scores
}
