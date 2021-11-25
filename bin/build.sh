#!/usr/bin/env bash

set -e
set -x

CI_PAGES_DOMAIN=${1}
CI_PAGES_URL=${2}
CI_PROJECT_TITLE=${3}
CI_PROJECT_URL=${4}
COMMIT_TIME=${5}
GITLAB_USER_NAME=${6}
GITLAB_USER_EMAIL=${7}
CI_COMMIT_SHA=${8}
CI_PROJECT_VISIBILITY=${9}

if ! type -p rsync >/dev/null ; then
  >&2 echo "Missing: rsync" >&2
fi

script_dir=$(dirname "$0")
dist_dir=${script_dir}/../public
src_dir=${script_dir}/../src
hs_dir=${script_dir}/..

mkdir -p ${dist_dir}

(pushd ${hs_dir} && cabal new-test && cabal new-build; popd)

mkdir -p ${dist_dir}
rsync -aH ${hs_dir}/dist-newstyle/ ${dist_dir}
