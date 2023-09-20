#!/usr/bin/env Rscript

# Access the internet from interactive nodes. UNSUPPORTED

Sys.setenv(http_proxy="http://172.28.7.1:3128")
Sys.setenv(https_proxy="http://172.28.7.1:3128")
Sys.setenv(all_proxy="http://172.28.7.1:3128")
Sys.setenv(no_proxy="localhost,*.chimera.hpc.mssm.edu,172.28.0.0/16")

message("You can now access the internet!")