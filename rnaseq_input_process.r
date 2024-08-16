#!/usr/bin/env Rscript

# Script to process input samplesheet and contrasts matrix for RNAseq analysis

library(optparse)
suppressPackageStartupMessages(library(dplyr))
library(readr)
library(stringr)
library(purrr)
library(tidyr)

option_list <- list(
  make_option(c("-s", "--input-samplesheet"),
              type = "character", help = "Input samplesheet", ),
  make_option(c("-c", "--input-contrasts"),
              type = "character", help = "Input contrasts matrix"),
  make_option(c("-d", "--dge"),
              type = "character", default = "deg_samplesheet.csv",
              help = "DGE samplesheet output"),
  make_option(c("-r", "--rnaseq"),
              type = "character", default = "samplesheet.csv",
              help = "RNAseq samplesheet output"),
  make_option(c("-C", "--contrasts"),
              type = "character", default = "contrasts.csv",
              help = "DGE contrasts matrix output")
)

parser <- OptionParser(
  usage = paste("usage: %prog -s [input_samplesheet.csv]",
                " -c [input_contrasts.csv] [[optional output files]]"),
  option_list = option_list)

arguments <- parser |>
  parse_args()

if (length(arguments[["input-samplesheet"]]) > 0) {
  arguments[["input_samplesheet"]] <- arguments[["input-samplesheet"]]
} else {
  # show usage and help if no input samplesheet provided
  print_help(parser)
  if (length(arguments[["input-contrasts"]]) == 0) {
    stop("Input samplesheet and contrasts not provided")
  }
  stop("Input samplesheet not provided")
}

if (!is.na(arguments[["input-contrasts"]])) {
  arguments[["input_contrasts"]] <- arguments[["input-contrasts"]]
} else {
  # show usage and help if no input contrasts matrix provided
  print_help(parser)
  stop("Input contrasts matrix not provided")
}

if (!file.exists(arguments$input_samplesheet)) {
  stop("Input samplesheet", arguments$input_samplesheet, "does not exist")
} else if (!file.exists(arguments$input_contrasts)) {
  stop("Input contrasts matrix", arguments$input_contrasts, "does not exist")
}

in_samplesheet <- arguments$input_samplesheet |>
  read_csv(col_types = cols(.default = "c"))
in_contrasts <- arguments$input_contrasts |>
  read_csv(col_types = cols(.default = "c"))

if ("strandedness" %in% colnames(in_samplesheet)) {
  valid_strandedness <- c("unstranded", "forward", "reverse", "auto")
  if (!all(in_samplesheet$strandedness %in% valid_strandedness)) {
    stop("Invalid strandedness in samplesheet: must be one of ",
    paste(valid_strandedness, collapse = ", "))
  }
  intermed_samplesheet_rnaseq <- in_samplesheet |>
    select(sample, fastq_1, fastq_2, strandedness)
  start_samplesheet_dge <- in_samplesheet |>
    select(-strandedness, -fastq_1, -fastq_2)
} else {
  intermed_samplesheet_rnaseq <- in_samplesheet |>
    select(sample, fastq_1, fastq_2) |>
    mutate(strandedness = "auto")
  start_samplesheet_dge <- in_samplesheet |>
    select(-fastq_1, -fastq_2)
}

# check for any duplicates in sample, fastq_1, or fastq_2
all_distinct <- function(df, colname) {
  dups <- df |>
    group_by(!!sym(colname)) |>
    filter(n() > 1) |>
    distinct(!!sym(colname)) |>
    pull(!!sym(colname))
  if (length(dups) > 0) {
    stop(paste("Duplicate", colname, "in samplesheet:",
         paste(dups, collapse = ", ")))
  }
}

all_distinct(intermed_samplesheet_rnaseq, "sample")
all_distinct(intermed_samplesheet_rnaseq, "fastq_1")
all_distinct(intermed_samplesheet_rnaseq, "fastq_2")

# check and fix paths in fastq_1 and fastq_2
get_common_path <- function(x) { # adapted from Biostrings lcPrefix
  if (all(x == x[[1]])) {
    return(x[[1]])
  }
  nc <- nchar(x)
  for (i in 1:min(nc)) {
    ss <- str_sub(x, 1, i)
    if (any(ss != ss[1])) {
      return(str_sub(x[1], 1, i - 1))
    }
  }
  str_sub(x[1], 1, i)
}

modify_path <- function(df, colname) {
  # all input fastq files must be in the "input" directory
  if (all(str_detect(df[[colname]], "^input/"))) {
    return(df)
  } else if (all(!str_detect(df[[colname]], "/"))) {
    # if there is no path before the filename, prepend "input/"
    df[[colname]] <- paste0("input/", df[[colname]])
  } else if (all(str_detect(df[[colname]], "/"))) {
    # if all paths are the same, remove the path and add "input/"
    # if there are different paths, replace common prefix with "input/" and
    #   print a warning containing the common path for symlinking
    prefixes <- str_extract(df[[colname]], ".+(?=/)")
    common_path <- get_common_path(prefixes)
    df[[colname]] <- str_replace(df[[colname]], common_path, "input")
    if (!all(prefixes == common_path) && colname == "fastq_1") {
      warning("Different paths in ", colname, " in samplesheet.\n",
              "  Common path is '", common_path, "' for symlinking.")
    }
  } else {
    stop("Unknown path format in", colname, "in samplesheet")
  }
  return(df)
}

test_fastq_stem <- function(df) {
  # check that the fastq_1 and fastq_2 files have the same stem
  mismatches <- df |>
    mutate(prefix_1 = str_extract(fastq_1, ".+(?=/)"),
           prefix_2 = str_extract(fastq_2, ".+(?=/)")) |>
    filter(prefix_1 != prefix_2) |>
    mutate(stopstr = sprintf("Different fastq stems for %s: %s and %s",
                             sample, prefix_1, prefix_2)) |>
    pull(stopstr) |>
    paste(collapse = "\n")

  if (mismatches != "") {
    stop(mismatches)
  }
  return(df)
}

out_samplesheet_rnaseq <- intermed_samplesheet_rnaseq |>
  modify_path("fastq_1") |>
  modify_path("fastq_2") |>
  test_fastq_stem() |>
  select(sample, fastq_1, fastq_2, strandedness)

# check contrast ID column is all distinct
all_distinct(in_contrasts, "id")

# check that the columns id,variable,reference,target,blocking are in contrasts
valid_contrasts_cols <- c("id", "variable", "reference", "target", "blocking")
if (!all(valid_contrasts_cols %in% colnames(in_contrasts))) { # all there
  stop("Contrasts matrix must contain columns: ",
       paste(valid_contrasts_cols, collapse = ", "))
} else if (!all(colnames(in_contrasts) %in% valid_contrasts_cols)) { # no others
  stop("Contrasts matrix must only contain columns: ",
       paste(valid_contrasts_cols, collapse = ", "))
}

# check that the reference and target are in the samplesheet
check_reftarg <- function(reftarg,
                          samplesheet = start_samplesheet_dge,
                          contrasts = in_contrasts) {
  # for each row in contrasts check that the reference and target are values
  #  in the column matching variable in the samplesheet
  miss <- contrasts |>
    filter(variable %in% colnames(samplesheet)) |>
    mutate(good = map2_lgl(!!sym(reftarg), variable,
                           \(r, v) r %in% samplesheet[[v]])) |>
    filter(!good) |>
    mutate(stopstr = sprintf("%s %s missing for contrast %s",
                             str_to_title(reftarg), !!sym(reftarg), id)) |>
    select(id, variable, !!sym(reftarg), stopstr)
  na <- contrasts |>
    filter(variable %in% colnames(samplesheet) & is.na(!!sym(reftarg))) |>
    mutate(stopstr = sprintf("%s undefined for contrast %s",
                             str_to_title(reftarg), id)) |>
    select(id, variable, !!sym(reftarg), stopstr)
  if (reftarg == "reference") {
    contrasts |>
      filter(!(variable %in% colnames(samplesheet))) |>
      mutate(stopstr = sprintf("Variable %s not in samplesheet for contrast %s",
                               variable, id)) |>
      select(id, variable, stopstr) |>
      bind_rows(miss, na)
  } else {
    bind_rows(miss, na)
  }
}

check_blocking <- function(samplesheet = start_samplesheet_dge,
                          contrasts = in_contrasts) {
  # for each row in contrasts check that the reference and target are values
  #  in the column matching variable in the samplesheet
  contrasts |>
    filter(!is.na(blocking) & nchar(blocking) != 0) |>
    separate_rows(blocking, sep = ";") |>
    mutate(good = blocking %in% colnames(samplesheet)) |>
    filter(!good) |>
    mutate(stopstr = sprintf("Blocking factor %s missing for contrast %s",
                             blocking, id)) |>
    select(id, blocking, stopstr)
}

bad_reference <- check_reftarg("reference")
bad_target <- check_reftarg("target")
missing_blocking <- check_blocking()

missing_txt <- bind_rows(bad_reference, bad_target, missing_blocking) |>
  pull(stopstr) |>
  paste(collapse = "\n")

if (missing_txt != "") {
  stop(missing_txt)
}

# write out the samplesheets and contrasts matrix

write_csv(out_samplesheet_rnaseq, arguments$rnaseq)
start_samplesheet_dge |>
  left_join(select(out_samplesheet_rnaseq, -strandedness),
            by = "sample") |>
  select(sample, fastq_1, fastq_2, everything()) |>
  mutate(sample = str_replace_all(sample, "-", ".")) |>
  write_csv(arguments$dge)
in_contrasts |>
  select(all_of(valid_contrasts_cols)) |>
  write_csv(arguments$contrasts, na = "")

message("Samplesheet and contrasts matrix processed successfully")
message("RNAseq samplesheet written to ", arguments$rnaseq)
message("DGE samplesheet written to ", arguments$dge)
message("Contrasts matrix written to ", arguments$contrasts)