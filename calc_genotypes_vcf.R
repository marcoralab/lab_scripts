#!/usr/bin/env Rscript
library(stringr)
library(readr)
library(tidyr)
library(tibble)
library(purrr)
suppressPackageStartupMessages(library(dplyr))

alleles_apoe <-
  tribble(~variant, ~ref, ~alt, ~chr, ~b38, ~hg19,
          "rs429358", "T", "C", 19, 44908684, 45411941,
          "rs7412", "C", "T", 19, 44908822, 45412079)

calc_apoe_genotype <- function(df) {
  mutate(df, APOE = case_when(
    is.na(rs7412) | is.na(rs429358) ~ NA_real_,
    grepl("0[/|]0|0[|]0", rs7412) & grepl("0[/|]0|0[|]0", rs429358) ~ 33,
    grepl("0[/|]1|1[|]0", rs7412) & grepl("0[/|]0|0[|]0", rs429358) ~ 23,
    grepl("1[/|]1|1[|]1", rs7412) & grepl("0[/|]0|0[|]0", rs429358) ~ 22,
    grepl("0[/|]1|1[|]0", rs7412) & grepl("0[/|]1|1[|]0", rs429358) ~ 24,
    grepl("0[/|]0|0[|]0", rs7412) & grepl("0[/|]1|1[|]0", rs429358) ~ 34,
    grepl("0[/|]0|0[|]0", rs7412) & grepl("1[/|]1|1[|]1", rs429358) ~ 44,
    TRUE ~ NA_real_
  ))
}

count_alleles <- function(df, min_count = 1) {
  count_allele <- . %>%
    str_extract_all("[0-2]") %>%
    map_dbl(~ sum(as.numeric(.x)))

  df |>
    mutate(mutate(across(any_of(unname(rsids)), count_allele))) %>%
    mutate(sum_dose = rowSums(select(., any_of(rsids)))) |>
    filter(sum_dose >= as.integer(min_count)) |>
    select(-sum_dose) 
}

helpstring <- paste(
  "A script to query genotypes from a VCF file\n",
  "Usage: get_genotypes [vcf file] [genome build] [[allele table]] [[minimum alleles]]\n",
  "Output: A tab separated file with columns for the sample ID (VCF_ID) and",
  "        each variant ID. The variant colums will have the number of",
  "        alterate alleles in each sample.\n",
  "Input (genome build): b38 or hg19\n",
  "Optional Input (allele table): a table with the variant name, ref allele,",
  "                               alternate allele, chromosome, build 38 pos,",
  "                               and hg19 pos. Will use APOE table below if",
  "                               not provided.\n",
  "Optional Input (minimum alleles): Minimum number of alleles for inculsion",
  "                                  of samples in output. Default is 1.",
  format_tsv(alleles_apoe),
  sep = "\n")


if (interactive()) {
  in_vcf <- paste0(
    "/sc/arion/projects/load/data-int/MSSM_ADRC/processed/2022-10-27/",
    "intermediate/imputation/imputed/processed/data/all_chrall_filtered.vcf.gz")
  alleles_raw <- alleles_apoe
  build <- "b38"
  calcfun <- calc_apoe_genotype
} else {
  arg <- commandArgs(TRUE)
  if (any(c("--help", "-h") %in% arg)) {
    cat(helpstring)
    q(save = "no")
  } else if (length(arg) == 2) {
    message("No variant table provided. Calculating APOE genotypes.")
    in_vcf <- arg[[1]]
    build <- arg[[2]]
    alleles_raw <- alleles_apoe
    calcfun <- calc_apoe_genotype
  } else if (length(arg) == 3) {
    in_vcf <- arg[[1]]
    build <- arg[[2]]
    alleles_raw <- arg[[3]] |>
      read_tsv(col_types = "ccccii")
    calcfun <- count_alleles
    min_alleles <- 1
  } else if (length(arg) == 4) {
    in_vcf <- arg[[1]]
    build <- arg[[2]]
    alleles_raw <- arg[[3]] |>
      read_tsv(col_types = "ccccii")
    calcfun <- function(x) count_alleles(x, min_count=arg[[4]])
  } else {
    cat(helpstring)
    stop("Incorrect number of arguments")
  }
}

in_tbi <- paste0(in_vcf, ".tbi")

if (!file.exists(in_tbi)) {
  message("Indexing VCF file...")
  system(paste("bcftools index -t", in_vcf))
}

alleles_build <-
  alleles_raw |>
  mutate(chrom_pos = paste(chr, .data[[build]], sep = ":"),
         chr_prefix = FALSE)

alleles <- alleles_build |>
  mutate(chrom_pos = paste0("chr", chrom_pos),
         chr_prefix = TRUE) |>
  bind_rows(alleles_build) %>%
  unite(cpra, chrom_pos, ref, alt, sep = ":", remove = F)

rsids <- alleles |>
  select(cpra, variant) |>
  tibble::deframe()

pos_bcftools <- paste(alleles$chrom_pos, collapse = ",")
query_format <- "%CHROM\t%POS\t%REF\t%ALT\t[%SAMPLE=%GT, ]\n"
bcftools <- sprintf("bcftools query -r '%s' -f '%s' %s",
                    pos_bcftools, query_format, in_vcf)

vartab <-
  system(command = bcftools, intern = TRUE) |>
  str_replace(", $", "") |>
  I() |>
  read_tsv(col_names = c("chromsome", "position", "ref", "alt", "genotype"),
           col_types = "ciccc") |>
  separate_rows(genotype, sep = ", ") |>
  separate(genotype, sep = "=", c("VCF_ID", "genotype")) |>
  #combine chromsome, position, ref, and alt into a new ID
  unite("CPRA", chromsome, position, ref, alt, sep = ":", remove = TRUE) |>
  pivot_wider(names_from = CPRA, values_from = genotype) |>
  rename_with( \(x) as_vector(rsids[x]), .cols = any_of(names(rsids)))

id_nogeno <- rsids[!(rsids %in% colnames(vartab))]

if (length(id_nogeno) > 0) {
  "%s out of %s variants in the input table were not in the VCF:" |>
    sprintf(length(id_nogeno), length(rsids)) |>
    write(stderr())
  "%s: %s" |>
    sprintf(id_nogeno, names(id_nogeno)) |>
    write(stderr())
  pos_noid <- colnames(vartab)[!(colnames(vartab) %in% c(unname(rsids), "VCF_ID"))]
  if (length(pos_noid) > 0) {
    "\n%s out of %s variants in the VCF did not match alleles in the input table:" |>
      sprintf(length(pos_noid), ncol(vartab) - 1) |>
      write(stderr())
    write(pos_noid, stderr())
  }
} else {
  message("All variants in the input table mapped to the TSV")
}

vartab |>
  select(VCF_ID, any_of(unname(rsids))) |>
  calcfun() |>
  format_tsv() |>
  cat()
