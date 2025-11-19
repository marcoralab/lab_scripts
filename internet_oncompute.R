#!/usr/bin/env Rscript

# Access the internet from interactive nodes. UNSUPPORTED

prox <- "proxy.chimera.hpc.mssm.edu:3128"

Sys.setenv(http_proxy=prox)
Sys.setenv(https_proxy=prox)
Sys.setenv(all_proxy=prox)
Sys.setenv(no_proxy="localhost,*.chimera.hpc.mssm.edu,172.28.0.0/16")

rm(prox)

message("You can now access the internet!")
