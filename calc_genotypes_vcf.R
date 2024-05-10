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

calc_apoe_genotype <- function(dfr) {
  if (all(grepl("|", dfr$rs7412)) & all(grepl("|", dfr$rs429358))) {
    final_calc <- function(rs429358, rs7412) {
      case_when(
        rs429358 == "C" & rs7412 == "T" ~ "1",
        rs429358 == "T" & rs7412 == "T" ~ "2",
        rs429358 == "T" & rs7412 == "C" ~ "3",
        rs429358 == "C" & rs7412 == "C" ~ "4",
        TRUE ~ NA_character_)
    }
    mutate(dfr,
      rs7412 = str_replace_all(rs7412, "0", "C"),
      rs7412 = str_replace_all(rs7412, "1", "T"),
      rs429358 = str_replace_all(rs429358, "0", "T"),
      rs429358 = str_replace_all(rs429358, "1", "C"),
      rs7412_1 = str_split(rs7412, "\\|", simplify = TRUE)[, 1],
      rs429358_1 = str_split(rs429358, "\\|", simplify = TRUE)[, 1],
      rs7412_2 = str_split(rs7412, "\\|", simplify = TRUE)[, 2],
      rs429358_2 = str_split(rs429358, "\\|", simplify = TRUE)[, 2],
      APOE_1 = final_calc(rs429358_1, rs7412_1),
      APOE_2 = final_calc(rs429358_2, rs7412_2)) |>
    rowwise() |>
    mutate(APOE = paste(sort(c(APOE_1, APOE_2)), collapse ="")) |>
    ungroup() |>
    select(-rs7412_1, -rs7412_2, -rs429358_1, -rs429358_2, -APOE_1, -APOE_2)
  } else if (all(grepl("/", dfr$rs7412)) & all(grepl("/", dfr$rs429358))) {
    mutate(dfr, APOE = case_when(
      is.na(rs7412) | is.na(rs429358) ~ NA_real_,
      grepl("0/0", rs7412) & grepl("0/0", rs429358) ~ "33",
      grepl("0/1", rs7412) & grepl("0/0", rs429358) ~ "23",
      grepl("1/1", rs7412) & grepl("0/0", rs429358) ~ "22",
      grepl("0/1", rs7412) & grepl("0/1", rs429358) ~ "24",
      grepl("0/0", rs7412) & grepl("0/1", rs429358) ~ "34",
      grepl("0/0", rs7412) & grepl("1/1", rs429358) ~ "44",
      TRUE ~ NA_character_
    ))
  } else {
    stop("Mixed phased and unphased genotypes. Please fix VCF.", call. = FALSE)
  }
}

count_alleles <- function(df, min_count = 0) {
  count_allele <- . %>%
    str_extract_all("[0-2]") %>%
    map_dbl(~ sum(as.numeric(.x)))

  df |>
    mutate(across(any_of(unname(rsids)), count_allele)) %>%
    mutate(sum_dose = rowSums(select(., any_of(rsids)))) |>
    filter(sum_dose >= as.integer(min_count)) |>
    select(-sum_dose)
}

base_alleles <- function(df) {
  base_alleles_col <- function(genotypes, varname) {
    var_row <- alleles_raw[alleles_raw["variant"] == varname, ]
    stopifnot(nrow(var_row) == 1)
    genotypes |>
      str_replace_all("0", var_row$ref) |>
      str_replace_all("1", var_row$alt)
  }
  df |>
    mutate(across(any_of(unname(rsids)), ~ base_alleles_col(.x, cur_column())))
}

helpstring <- paste(
  "A script to query genotypes from a VCF file\n",
  "Usage: get_genotypes [vcf file] [genome build] [[allele table]] [[allele inclusion]]\n",
  "Output: A tab separated file with columns for the sample ID (VCF_ID) and",
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
  in_vcf <- paste0(
    "/sc/arion/projects/load/data-int/MSSM_ADRC/processed/2022-10-27/",
    "intermediate/imputation/imputed/processed/data/all_chrall_filtered.vcf.gz")
  alleles_raw <- alleles_apoe
  build <- "b38"
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
    in_vcf <- arg[[1]]
    build <- arg[[2]]
    alleles_raw <- alleles_apoe
    calcfun <- calc_apoe_genotype
  } else if (length(arg) == 3) {
    in_vcf <- arg[[1]]
    build <- arg[[2]]
    alleles_raw <- arg[[3]] |>
      read_tsv(col_types = coltypes)
    calcfun <- count_alleles
  } else if (length(arg) == 4 && arg[[4]] %in% c("a", "alleles")) {
    in_vcf <- arg[[1]]
    build <- arg[[2]]
    alleles_raw <- arg[[3]] |>
      read_tsv(col_types = coltypes)
    calcfun <- base_alleles
  } else if (length(arg) == 4) {
    in_vcf <- arg[[1]]
    build <- arg[[2]]
    alleles_raw <- arg[[3]] |>
      read_tsv(col_types = coltypes)
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
