#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(dplyr))
library(readr)
library(stringr)

options(width = as.integer(system("tput cols", intern = TRUE)) - 5)

ncores <- function(x) {
  y <- str_extract_all(x, "\\d+(?=[*])") %>% sapply(function (x) sum(as.integer(x)))
  ifelse(y == 0, 1, y)
}

jobs <- "bjobs -u all -o \"jobid job_name user proj_name queue stat cpu_used nthreads exec_host time_left cpu_efficiency delimiter='^'\" " %>%
  pipe %>%
  read_delim(col_types = cols(
    .default = col_character(),
    JOBID = col_double(),
    NTHREADS = col_double(),
    ), delim = "^", na = "-") %>%
  mutate(ncore = ncores(EXEC_HOST),
         CPU_EFFICIENCY = parse_number(CPU_EFFICIENCY) / 100,
         nodes = str_count(EXEC_HOST, ":") + 1)

message("Users")
ujobs <- jobs %>%
  filter(STAT == "RUN") %>%
  group_by(USER) %>%
  summarise(
    ncores = sum(ncore),
    njobs = n(),
    avgcores = mean(ncore),
    cpu = sprintf("%.1f%%", mean(CPU_EFFICIENCY) * 100),
    avgnodes = mean(nodes),
    .groups = "drop")

paste(c("COLUMNS=400 finger -s", ujobs$USER,
        "| sed -r 's/[[:blank:]]+/\\t/;s/\\s\\s+.+//'"),
      collapse = " ") %>%
  pipe %>%
  read_tsv(col_types = "cc") %>%
  distinct %>%
  right_join(ujobs, by = c("Login" = "USER")) %>%
  arrange(-ncores) %>%
  print(n = Inf)

message("\nProjects")
jobs %>% 
  filter(STAT == "RUN") %>% 
  group_by(PROJ_NAME) %>%
  summarise(
    ncores = sum(ncore),
    njobs = n(),
    avgcores = mean(ncore),
    users = n_distinct(USER),
    cpu = sprintf("%.1f%%", mean(CPU_EFFICIENCY) * 100),
    avgnodes = mean(nodes),
    .groups = "drop") %>%
  arrange(-ncores) %>%
  print(n = Inf)
