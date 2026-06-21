# =====================================================================
# install_packages.R  —  one-off dependency install. Run once.
# =====================================================================
pkgs <- c(
  "tidyverse", "here", "digest",      # core + hashing
  "readxl",                            # O*NET .xlsx
  "httr2", "jsonlite",                 # API scoring
  "furrr", "future",                   # parallel sync calls
  "fixest", "modelsummary",            # regressions + tables
  "rlang"
)
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
message("dependencies ready: ", paste(pkgs, collapse = ", "))
