#!/bin/bash

#!/usr/bin/env bash

set -e
set -x

script_dir=$(dirname "$0")
hs_dir=${script_dir}/..

(pushd ${hs_dir} && cabal new-update && cabal new-configure; popd)
