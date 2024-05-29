# Goate and Marcora Lab Scripts

All of these scripts are available on Minerva in `/sc/arion/projects/load/scripts` and in the default `PATH`s of lab members, but can also be downloaded separately.

##  `ijob` and `regjob`

These are job submission scripts with overridable sane defaults for the Minerva LSF cluster. The scripts will also automatically choose queues and add `-R himem` based on time and memory requirements, as well as enabling internet within the jobs.

`ijob` starts an interactive job, whereas `regjob` starts a standard, noninteractive job.

By default, these scripts use the `acc_LOAD` account, so be sure to specify project if you cannot use `acc_LOAD` or do not want to.

### `regjob -h`

```
usage: regjob [supported args] [unsupported args] [command]
-n    | --num_cores         (4)                   Cores to request
-W    | --walltime          (140:00)              Time to request
-m    | --mem               (4000)                Memory per core (MB)
-q    | --queue             (automatic)           LSF Queue
-P    | --project           (acc_LOAD)            LSF Project
-h    | --help                                    Brings up this menu
```

### `ijob -h`

```
usage: ijob [supported args] [unsupported args]
-n    | --num_cores         (4)                   Cores to request
-W    | --walltime          (140:00)              Time to request
-m    | --mem               (4000)                Memory per core (MB)
-q    | --queue             (automatic)           LSF Queue
-P    | --project           (acc_LOAD)            LSF Project
-h    | --help                                    Brings up this menu
```

## Internet on compute nodes

By default, there is no internet at all on compute nodes. There are several ways to enable HTTP, HTTPS, FTP and Rsync access by setting proxy variables. ***There is no way to enable SSH on compute nodes***, so that means no SSH Github pushes, pulls, fetches, merges or clones.

`regjob` and `ijob` will enable HTTP and HTTPS proxies by default, or you can always run `internet_oncompute.{sh,fish,R}` depending on scripting language.

If you have access to the module system, you can also just run `ml proxies`.

| Context | Command |
| ---: | :--- |
| any shell with modules | `ml proxies` |
| bash/zsh/sh | `. internet_oncompute.sh` |
| fish shell | `. internet_oncompute.fish` |
| R | `source("/sc/arion/projects/load/scripts/internet_oncompute.R")` |

## Whiteout scripts

`whiteout` and `whiteout_tab` are `sed` scripts to fix the whitespace in text files. Both scripts remove leading and trailing whitespace.

`whiteout` then replaces all runs of one or more whitespace characters with a single space, whereas `whiteout_tab` replaces all runs of one or more whitespace characters with a single tab.

Usage is `whiteout <file> > <output>` or `whiteout_tab <file> > <output>`.

## Conda yaml script

`conda_yaml_build` performs various tasks to create and maintain conda environment yaml files. It can create a new yaml file or update an existing yaml file.

The script works by actually creating the environment to get versions of packages and ensure there are no conflicts. The environment (`test__test__test`) is removed when the script is done.

```
Script Usage: conda_yaml_build [options]

Options:
  -h, --help          Display this help information
  -v, --version       Print the script version number
  -d, --directory     Specify the directory to search for R package dependencies
  -y, --yaml          Specify an existing YAML file with R package dependencies
  --no-ver            Skip version retrieval and only perform validation
  --no-val            Skip validation and only retrieve versions
  -s, --strip-extra   Remove packages not specifically loaded or in yaml
  -u, --update        Update all packages in YAML to the latest possible version

Use --directory or --yaml. Or you can use neither to search the current directory.
```

### Creating a new yaml file

`conda_yaml_build -d /path/to/R/packages/ > environment.yaml`

In order to create a new environment yaml file, the script can recursively scan a directory for all R files and infer the package dependencies based on `library()`, `require()`, `packload()`, and `p_load()` as well as explicit invocations of functions using `package::function()`. Packages in the R standard library are ignored. The `r-tidyverse` package is automatically added to the yaml file if it is not already present and `dplyr` is used.

The script automatically detects if the package is from CRAN or Bioconductor and will add the appropriate prefixes to the package names. The script will also print a warning if a package is not found in the CRAN or Bioconductor repositories.

The script will retrieve the latest version of each package that works with the other packages and output the yaml file.

`-s` or `--strip-extra` is particularly useful here if you do not want to include packages that are not explicitly loaded in the R files.

`-d` or `--directory` can be omitted if you want to search the current directory.

If you want to add non-R packages, you can manually add them to the yaml file after the script creates it, but it is then recommented to rerun the script with `-y <yaml file> -u` to update all packages to the latest version and insure there are no conflicts (see below).

### Updating an existing yaml file

`conda_yaml_build -y environment.yaml -u > environment_updated.yaml`

In order to update an existing environment yaml file, the script will create a new environment with the packages in the yaml file and then update all packages to the latest version that works with the other packages. The script will output the updated yaml file.

`-u` or `--update` is required to update the packages in the yaml file.

`-s` or `--strip-extra` can be used to remove packages not specifically loaded in the R files or in the yaml file.

### Other scripts

`conda_yaml_upd`, `conda_yaml_upd_latest`, and `conda_yaml_find-rdeps` are provided for historical reasons, are not recommended for use, and may be removed in the future.

## bblame scripts

The bblame scripts are useful to understand who is using the cluster, how efficiently they are using it, and therefore if and how the cluster is being "slammed". These scripts only show running jobs and cannot give an idea of how clogged the queue is.

`bblame` or `bblame.R` will show the current usage of the cluster, including the number of jobs, the number of cores, and cpu efficiency. Users and projects are sorted by usage. The script does not distinguish between regular jobs and himem or long jobs.

`bblame_long` looks only at long jobs and does not calculate efficiency.

`bblame_himem` looks only at himem jobs and calculates memory efficiency instead.

## Genotype calculation scripts

By default, these scripts determine the APOE genotype from either an indexed vcf/bcf/vcf.gz file or PLINK 1.9 binary fileset. If provided with an allele table, the scripts can extract genotypes for other arbitrary variants.

### Requirements

`calc_genotypes_vcf.R` requires `bcftools` to be in the `PATH`. `calc_genotypes_plink.R` requires the `plink` binary to be in the `PATH`.

You always need to specify the genotype file or fileset and the genome build (b38 or hg19). You also must specify an allele table if you want to extract genotypes for non-*APOE* variants.

### Allele table

Allele tables for non-*APOE* variants must be tab-separated, with the following header line:
    
```text
variant ref alt chr b38 hg19
```

The `variant` column should contain the variant name, `ref` and `alt` should contain the reference and alternate alleles, `chr` should contain the chromosome name (without `chr`), and `b38` and `hg19` should contain the positions in build 38 and hg19, respectively.

The final table should look something like this:
    
```text
variant	ref	alt	chr	b38	hg19
rs429358	T	C	19	44908684	45411941
rs7412	C	T	19	44908822	45412079
```

### Usage

#### `calc_genotypes_vcf.R`

```text
A script to query genotypes from a VCF file

Usage: calc_genotypes_vcf.R [vcf file] [genome build] [[allele table]] [[allele inclusion]]

Output: A tab separated file with columns for the sample ID (VCF_ID) and
        each variant ID. The variant colums will have the number of
        alterate alleles in each sample by default or the actual alleles if requested.

Input (genome build): b38 or hg19

Optional Input (allele table): a table with the variant name, ref allele,
                               alternate allele, chromosome, build 38 pos,
                               and hg19 pos. Will use APOE table below if
                               not provided.

Optional Input (allele inclusion): Minimum number of alleles for inculsion if number
                                   of samples in output. Default is 0.
                                   If 'a' or 'alleles', show actual alleles instead
                                   of allele count.


This is the default allele table:
variant	ref	alt	chr	b38	hg19
rs429358	T	C	19	44908684	45411941
rs7412	C	T	19	44908822	45412079
```

#### `calc_genotypes_plink.R`

```text
A script to query genotypes from a PLINK fileset

Usage: calc_genotypes_plink.R [plink stem] [genome build] [[allele table]] [[allele inclusion]]

Output: A tab separated file with columns for the FID, IID and
        each variant ID. The variant colums will have the number of
        alterate alleles in each sample by default or the actual alleles if requested.

Input (genome build): b38 or hg19

Optional Input (allele table): a table with the variant name, ref allele,
                               alternate allele, chromosome, build 38 pos,
                               and hg19 pos. Will use APOE table below if
                               not provided.

Optional Input (allele inclusion): Minimum number of alleles for inculsion if number
                                   of samples in output. Default is 0.
                                   If 'a' or 'alleles', show actual alleles instead
                                   of allele count.


This is the default allele table:
variant ref     alt     chr     b38     hg19
rs429358        T       C       19      44908684        45411941
rs7412  C       T       19      44908822        45412079
```
