# Goate and Marcora Lab Scripts

All of these scripts are available on Minerva in `/sc/arion/projects/load/scripts` and in the default `PATH`s of lab members, but can also be downloaded separately.

##  `ijob` and `regjob`

These are job submission scripts with overridable sane defaults for the Minerva LSF cluster. The scripts will also automatically choose queues and add `-R himem` based on time and memory requirements.

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
