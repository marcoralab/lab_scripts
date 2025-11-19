#!/usr/bin/env fish

set prox proxy.chimera.hpc.mssm.edu:3128
set -U http_proxy "$prox"
set -U https_proxy "$prox"
set -U ftp_proxy "$prox"
set -U rsync_proxy "$prox"
set -U all_proxy "$prox"
set -U no_proxy "localhost,*.chimera.hpc.mssm.edu,172.28.0.0/16"

echo You now have internet access.
echo run nointernet_oncompute.fish to undo
