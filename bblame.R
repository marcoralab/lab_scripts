#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(dplyr))
library(readr)
library(stringr)

options(width = as.integer(system("tput cols", intern = TRUE)) - 5)

ncores <- function(x) {
  y <- str_extract_all(x, "\\d+(?=[*])") |>
    sapply(\(x) sum(as.integer(x)))
  ifelse(y == 0, 1, y)
}

jobfilter <- function(df) filter(df, STAT == "RUN")

# library(purrr)
# all_jobnames <- "bjobs -u all -o job_name" |> system(intern = TRUE)
#
# delim_use <-
#   tibble(delim = c("^", "&", "$", "?"),
#          re = c("\\^", "&", "\\$", "\\?")) |>
#   mutate(count = map_int(re, \(x) length(str_subset(all_jobnames, x)))) |>
#   arrange(count) |>
#   pull(delim) |>
#   head(1)
#
# cols_bjobs <- paste(
#   "jobid", "nthreads", "job_name", "user", "proj_name", "queue", "stat",
#   "cpu_used", "exec_host", "time_left", "cpu_peak_efficiency"
# )

delim_use <- "^"
cols_bjobs <- paste(
  "jobid", "nthreads", "user", "proj_name", "queue", "stat",
  "cpu_used", "exec_host", "time_left", "cpu_peak_efficiency"
)

jobs <-
  "bjobs -u all -o \"%s delimiter='%s'\" " |>
  sprintf(cols_bjobs, delim_use) |>
  pipe() |>
  read_delim(col_types = cols(
    .default = col_character(),
    JOBID = col_double(),
    NTHREADS = col_double(),
  ), delim = delim_use, na = "-") |>
  mutate(ncore = ncores(EXEC_HOST),
         CPU_EFFICIENCY = parse_number(CPU_PEAK_EFFICIENCY) / 100,
         nodes = str_count(EXEC_HOST, ":") + 1)

message("Users")
ujobs <- jobs |>
  jobfilter() |>
  group_by(USER) |>
  summarise(
    ncores = sum(ncore),
    njobs = n(),
    avgcores = mean(ncore),
    cpu = sprintf("%.1f%%", mean(CPU_EFFICIENCY) * 100),
    avgnodes = mean(nodes),
    .groups = "drop")

c("getent passwd", ujobs$USER) |>
  paste(collapse = " ") |>
  pipe() |>
  read_delim(delim = ":", col_types = "c---c--",
             col_names = c("Login", "Name")) |>
  distinct() |>
  right_join(ujobs, by = c("Login" = "USER")) |>
  arrange(-ncores) |>
  print(n = Inf)

message("\nProjects")
jobs |>
  filter(STAT == "RUN") |>
  group_by(PROJ_NAME) |>
  summarise(
    ncores = sum(ncore),
    njobs = n(),
    avgcores = mean(ncore),
    users = n_distinct(USER),
    cpu = sprintf("%.1f%%", mean(CPU_EFFICIENCY) * 100),
    avgnodes = mean(nodes),
    .groups = "drop") |>
  arrange(-ncores) |>
  print(n = Inf)
