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
