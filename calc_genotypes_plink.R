#!/usr/bin/env Rscript
library(stringr)
library(readr)
library(tidyr)
library(tibble)
library(purrr)
suppressPackageStartupMessages(library(dplyr))

alleles_apoe <-
  tribble(~variant, ~ref, ~alt, ~chr, ~b38, ~hg19,
          "rs429358", "T", "C", "19", 44908684, 45411941,
          "rs7412", "C", "T", "19", 44908822, 45412079)

count_alleles <- function(df, min_count = 0) {
  count_allele <- function(x) {
    alt <- alleles |>
      filter(variant == cur_column()) |>
      pull(alt)
    # count occurrences of alternate allele in each element
    str_count(x, alt)
  }

  ids <- df |>
    select(-FID, -IID) |>
    colnames()

  df |>
    mutate(across(any_of(ids), count_allele)) %>%
    mutate(sum_dose = rowSums(select(., any_of(ids)))) |>
    filter(sum_dose >= as.integer(min_count)) |>
    select(-sum_dose)
}

calc_apoe_genotype <- function(dfr) {
  dfr |>
    count_alleles() |>
    mutate(APOE = case_when(
      is.na(rs7412) | is.na(rs429358) ~ NA_character_,
      rs7412 == 0 & rs429358 == 0 ~ "33",
      rs7412 == 1 & rs429358 == 0 ~ "23",
      rs7412 == 2 & rs429358 == 0 ~ "22",
      rs7412 == 1 & rs429358 == 1 ~ "24",
      rs7412 == 0 & rs429358 == 1 ~ "34",
      rs7412 == 0 & rs429358 == 2 ~ "44",
      TRUE ~ NA_character_
    ))
}

helpstring <- paste(
  "A script to query genotypes from a PLINK file\n",
  "Usage: calc_genotypes_plink.R [plink stem] [genome build] [[allele table]] [[allele inclusion]]\n",
  "Output: A tab separated file with columns for the FID, IID and",
  "        each variant ID. The variant colums will have the number of",
  "        alterate alleles in each sample by default or the actual alleles if requested.\n",
  "Input (genome build): b38 or hg19\n",
  "Optional Input (allele table): a table with the variant name, ref allele,",
  "                               alternate allele, chromosome, build 38 pos,",
  "                               and hg19 pos. Will use APOE table below if",
  "                               not provided.\n",
  "Optional Input (allele inclusion): Minimum number of alleles for inculsion if number",
  "                                   of samples in output. Default is 0.",
  "                                   If 'a' or 'alleles', show actual alleles instead",
  "                                   of allele count.\n\n",
  "This is the default allele table:",
  format_tsv(alleles_apoe),
  sep = "\n")

if (interactive()) {
  in_plink <- paste0(
    "/sc/arion/projects/load/users/fultob01/projects/UCI/",
    "raw/GSA_Irvine_imputed_QC")
  alleles_raw <- alleles_apoe
  build <- "hg19"
  calcfun <- calc_apoe_genotype
} else {
  arg <- commandArgs(TRUE)
  coltypes <- cols(.default = "?", variant = "c", ref = "c", alt = "c",
                    chr = "c", b38 = "i", hg19 = "i")
  if (any(c("--help", "-h") %in% arg)) {
    cat(helpstring)
    q(save = "no")
  } else if (length(arg) == 2) {
    message("No variant table provided. Calculating APOE genotypes.")
    in_plink <- arg[[1]]
    build <- arg[[2]]
    alleles_raw <- alleles_apoe
    calcfun <- calc_apoe_genotype
  } else if (length(arg) == 3) {
    in_plink <- arg[[1]]
    build <- arg[[2]]
    alleles_raw <- arg[[3]] |>
      read_tsv(col_types = coltypes)
    calcfun <- count_alleles
  } else if (length(arg) == 4 && arg[[4]] %in% c("a", "alleles")) {
    in_plink <- arg[[1]]
    build <- arg[[2]]
    alleles_raw <- arg[[3]] |>
      read_tsv(col_types = coltypes)
    calcfun <- identity
  } else if (length(arg) == 4) {
    in_plink <- arg[[1]]
    build <- arg[[2]]
    alleles_raw <- arg[[3]] |>
      read_tsv(col_types = coltypes)
    calcfun <- function(x) count_alleles(x, min_count = arg[[4]])
  } else {
    cat(helpstring)
    stop("Incorrect number of arguments")
  }
}

alleles_noplink <-
  alleles_raw |>
  mutate(ref = toupper(ref),
         alt = toupper(alt),
         pos = .data[[build]],
         palendromic = ((ref == "A" & alt == "T") |
                        (ref == "T" & alt == "A") |
                        (ref == "G" & alt == "C") |
                        (ref == "C" & alt == "G"))) |>
  select(-b38, -hg19)

fliptab <- list(
  "A" = "T",
  "T" = "A",
  "C" = "G",
  "G" = "C"
)

variants_plink <-
  paste0(in_plink, ".bim") |>
  read_table(col_names = c("chr", "plink_id", "pos", "a1", "a2"),
             col_types = "cc-icc") |>
  mutate(a1_flipped = map_chr(a1, ~ fliptab[[.x]]),
         a2_flipped = map_chr(a2, ~ fliptab[[.x]]))

alleles_plink_aligned <-
  variants_plink |>
  inner_join(alleles_noplink, by =
             join_by(chr == chr, pos == pos, a1 == alt, a2 == ref)) |>
  mutate(flipping = "aligned") |>
  select(-a1_flipped, -a2_flipped, -palendromic) |>
  left_join(alleles_noplink, by = join_by(chr, pos, variant))

alleles_plink_switched <-
  variants_plink |>
  inner_join(alleles_noplink, by =
             join_by(chr == chr, pos == pos, a1 == ref, a2 == alt)) |>
  mutate(flipping = "switched") |>
  select(-a1_flipped, -a2_flipped, -palendromic) |>
  left_join(alleles_noplink, by = join_by(chr, pos, variant))

alleles_plink_flipped <-
  variants_plink |>
  inner_join(alleles_noplink, by =
             join_by(chr == chr, pos == pos,
                     a1_flipped == alt, a2_flipped == ref)) |>
  mutate(flipping = "flipped") |>
  select(-a1_flipped, -a2_flipped, -palendromic) |>
  left_join(alleles_noplink, by = join_by(chr, pos, variant))

alleles_plink_flipped_switched <-
  variants_plink |>
  inner_join(alleles_noplink, by =
             join_by(chr == chr, pos == pos,
                     a1_flipped == ref, a2_flipped == alt)) |>
  mutate(flipping = "flipped_switched") |>
  select(-a1_flipped, -a2_flipped, -palendromic) |>
  left_join(alleles_noplink, by = join_by(chr, pos, variant))

alleles <-
  bind_rows(alleles_plink_aligned, alleles_plink_switched,
            alleles_plink_flipped, alleles_plink_flipped_switched) |>
  group_by(chr, pos, ref, alt) |>
  mutate(match_n = n()) |>
  ungroup()

unmatched <-
  alleles_noplink |>
  anti_join(alleles, by = join_by(chr, pos, ref, alt))

id_nogeno <- unmatched$variant

format_list <- function(lst) {
  if (length(lst) > 1) {
    paste(paste(lst[-length(lst)], collapse = ", "), "and", lst[length(lst)])
  } else {
    as.character(lst)
  }
}

if (any(alleles$match_n > 1)) {
  alleles |>
    filter(match_n > 1) |>
    format_csv() |>
    message()
  stop("Duplicate alleles found. See above for details.")
} else {
  match_issues <- FALSE
  if (nrow(unmatched) > 0) {
    "%s out of %s variants in the input table were not in the .fam file:" |>
      sprintf(length(id_nogeno), nrow(alleles_noplink)) |>
      write(stderr())
    id_nogeno |>
      format_list() |>
      write(stderr())
    match_issues <- TRUE
  }
  if (any(alleles$palendromic)) {
    palendromic_vars <- alleles |>
      filter(palendromic) |>
      distinct(variant) |>
      pull(variant) |>
      format_list()
    warning(sprintf("Palendromic alleles found: %s", palendromic_vars))
    match_issues <- TRUE
  }
  if (!match_issues) {
    message("All variants in the input table mapped to the input table.")
  }
}

# random string 5 char
randstring <- paste(sample(c(letters, LETTERS, 0:9),
                           5, replace = TRUE),
                    collapse = "")
tdir <- sprintf("/tmp/temp_calcg_%s", randstring) #tempdir()
dir.create(tdir, showWarnings = FALSE)
plink_textstem <- file.path(tdir, "varquery")
plink_command_txtfile <- sprintf(
  "plink %s --snps %s --bfile %s --out %s --recode compound-genotypes 1>&2",
  "--keep-allele-order", paste(alleles$plink_id, collapse = ", "),
  in_plink, plink_textstem
)

system(plink_command_txtfile)

pmap <-
  file.path(tdir, "varquery.map") |>
  read_table(col_names = c("chr", "plink_id", "pos"),
             col_types = "cc-i") |>
  left_join(alleles, by = join_by(chr, pos, plink_id))

stopifnot(all(!is.na(pmap$variant)))

mapnms <- pmap$variant

ped <-
  file.path(tdir, "varquery.ped") |>
  read_table(col_names = c("FID", "IID", "disc1", "disc2",
                           "disc3", "disc4", mapnms),
             col_types = cols(.default = "c")) |>
  select(-starts_with("disc"))

ped |>
  calcfun() |>
  format_tsv() |>
  cat()

# Clean up
disc <- list.files(tdir, pattern = "varquery", full.names = TRUE) |>
  file.remove()
