#!/usr/bin/env bash

prox=proxy.chimera.hpc.mssm.edu:3128
export http_proxy="$prox"
export https_proxy="$prox"
export ftp_proxy="$prox"
export rsync_proxy="$prox"
export all_proxy="$prox"
export no_proxy="localhost,*.chimera.hpc.mssm.edu,172.28.0.0/16"

echo You now have internet access.
