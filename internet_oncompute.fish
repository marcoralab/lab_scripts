#!/usr/bin/env fish

set -U http_proxy http://172.28.7.1:3128
set -U https_proxy http://172.28.7.1:3128
set -U all_proxy http://172.28.7.1:3128
set -U no_proxy localhost,*.chimera.hpc.mssm.edu,172.28.0.0/16

echo You now have internet access.
echo run nointernet_oncompute.fish to undo
