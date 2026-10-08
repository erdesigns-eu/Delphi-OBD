#!/usr/bin/env bash
# Build the pinned official FPC compiler and genuine runtime packages.
set -euo pipefail
fpc_source_dir="${1:-/tmp/erd-fpc-source}"
fpc_bootstrap="${FPC_BOOTSTRAP_COMPILER:-$(command -v fpc || true)}"
fpc_revision=2933f5ca60d86087451a1f8d5c726bc1c7ed5dff
if [[ -z "$fpc_bootstrap" ]]; then
  echo 'Install FPC 3.2.2 or set FPC_BOOTSTRAP_COMPILER to its executable.' >&2
  exit 1
fi
if [[ ! -d "$fpc_source_dir/.git" ]]; then
  git clone --depth 1 --no-checkout https://github.com/fpc/FPCSource.git "$fpc_source_dir"
fi
git -C "$fpc_source_dir" fetch --depth 1 origin "$fpc_revision"
git -C "$fpc_source_dir" checkout --detach "$fpc_revision"
fpc_source_dir="$(cd "$fpc_source_dir" && pwd)"
fpc_bootstrap="$(command -v "$fpc_bootstrap")"
make -C "$fpc_source_dir" -j4 compiler_cycle "FPC=$fpc_bootstrap" OPT=-O2
# compiler_cycle's temporary RTL is not the final package-build RTL.
make -C "$fpc_source_dir" rtl_clean "FPC=$fpc_source_dir/compiler/ppcx64"
make -C "$fpc_source_dir" -j4 rtl "FPC=$fpc_source_dir/compiler/ppcx64" OPT=-O2
make -C "$fpc_source_dir" -j4 packages "FPC=$fpc_source_dir/compiler/ppcx64" OPT=-O2
printf 'FPC source tree ready: %s\n' "$fpc_source_dir"
