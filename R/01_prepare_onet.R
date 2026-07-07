# =====================================================================
# 01_prepare_onet.R  —  Build the task dataset
# Loads O*NET v30.3, filters to Importance ratings, drops suppressed
# tasks, attaches occupation titles.
# =====================================================================

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages(library(readxl))

f_tasks   <- require_file(file.path(PATHS$onet, "Task Statements.xlsx"),
                          "Download O*NET v30.3 from onetcenter.org/database.aspx")
f_ratings <- require_file(file.path(PATHS$onet, "Task Ratings.xlsx"))
f_occ     <- require_file(file.path(PATHS$onet, "Occupation Data.xlsx"))

# --- Standardise column names
clean_names <- function(df) {
  names(df) <- names(df) |>
    str_replace_all("\\*", "") |>
    str_trim() |>
    str_replace_all("[^A-Za-z0-9]+", "_") |>
    str_to_lower() |>
    str_replace_all("^_|_$", "")
  df
}

tasks   <- read_excel(f_tasks)   |> clean_names()
ratings <- read_excel(f_ratings) |> clean_names()
occ     <- read_excel(f_occ)     |> clean_names()

# Expected key columns
ren <- function(df, map) {
  for (nm in names(map)) if (map[[nm]] %in% names(df)) df <- rename(df, !!nm := !!map[[nm]])
  df
}
tasks   <- ren(tasks,   c(onet_soc_code = "onetsoc_code", task_id = "task_id",
                          task = "task", task_type = "task_type"))
ratings <- ren(ratings, c(onet_soc_code = "onetsoc_code", task_id = "task_id",
                          scale_id = "scale_id", data_value = "data_value",
                          recommend_suppress = "recommend_suppress"))
occ     <- ren(occ,     c(onet_soc_code = "onetsoc_code", title = "title"))

# --- Importance ratings only (Scale ID == "IM"), drop suppressed
im <- ratings |>
  filter(scale_id == "IM") |>
  mutate(recommend_suppress = coalesce(recommend_suppress, "N")) |>
  filter(recommend_suppress != "Y") |>
  transmute(onet_soc_code, task_id, importance = as.numeric(data_value))

task_df <- tasks |>
  inner_join(im, by = c("onet_soc_code", "task_id")) |>
  left_join(select(occ, onet_soc_code, occupation_title = title),
            by = "onet_soc_code") |>
  filter(!is.na(task), importance >= 1, importance <= 5) |>
  distinct(onet_soc_code, task_id, .keep_all = TRUE)

# --- Guard: no task may reach the scorer without an occupation title,
#     since the title conditions the model's judgement.
n_no_title <- sum(is.na(task_df$occupation_title))
if (n_no_title > 0)
  stop(n_no_title, " tasks have no occupation title (Occupation Data join ",
       "failed for their O*NET-SOC codes). Fix before scoring.", call. = FALSE)

# --- Coverage sanity: flag occupations with < 3 retained tasks
thin <- task_df |> count(onet_soc_code) |> filter(n < 3)
if (nrow(thin)) message("note: ", nrow(thin),
                        " occupations have <3 tasks (flag in data notes).")

message("task-occupation pairs: ", nrow(task_df),
        " across ", n_distinct(task_df$onet_soc_code), " occupations")

save_csv(task_df, file.path(PATHS$cache, "task_df.csv"))
