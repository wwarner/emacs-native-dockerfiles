#! /bin/bash

cd "$(dirname "$(realpath "$0")")"/../modes

USAGE="build.sh <mode> [mode]"
if [ "$#" == "0" ];
then
    echo "$USAGE" > /dev/stderr
    exit 1
fi

DOCKER_TAG_SUFF=""
MODES_AVAILABLE=($(ls))
MODES=("${MODES_AVAILABLE[@]}")

if [ "$1" != "all" ]
then
    MODES=("$@")
    DOCKER_TAG_SUFF=-$(printf '%s-' "${MODES[@]}") # join args
    DOCKER_TAG_SUFF=${DOCKER_TAG_SUFF%-} # Remove trailing dash
    DOCKER_TAG_SUFF=$(echo "$DOCKER_TAG_SUFF" | tr '/_' '-' | tr '[:upper:]' '[:lower:]')
fi
DOCKER_TAG="emacs-native${DOCKER_TAG_SUFF}"

set -eu -o pipefail

EMACS_BRANCH="${EMACS_BRANCH:=30.2}"
FP=v"${EMACS_BRANCH}"-"$(printf '%04d' "$(git rev-list --count --no-merges HEAD)")"-"$(git rev-parse --short HEAD)"
ARCH=$(uname -m)
BASE=${FP}-${ARCH}
DOCKERFILE=".Dockerfile-${DOCKER_TAG}"

# Top
cat > "$DOCKERFILE" <<EOF
# Copyright 2024 William Warner
# SPDX-License-Identifier: GPL-3.0-only
# file generated with build.sh $@
ARG BASE="${BASE}"
FROM emacs-native

EOF

# Middle
for d in "${MODES[@]}"; do
    part="${d}/Dockerfile.part"
    { echo "#"; echo "# ${part}"; cat "$part"; } >> "${DOCKERFILE}"
done

# Bottom
cat >> "${DOCKERFILE}" <<EOF

RUN emacs -Q --script ".emacs.d/init.el"

CMD ["emacs"]
EOF

BUILDKIT_PROGRESS=plain docker build -t "${DOCKER_TAG}" -f "${DOCKERFILE}" .
echo "sample invocation:"
cat<<_EOF
  docker run -it --rm \\
    --name "${DOCKER_TAG}" \\
    -v\$HOME/src:/root/src \\
    -v\$HOME/.gitconfig:/etc/gitconfig \\
    -v\$HOME/.ssh:/root/.ssh \\
    -v\$HOME/.aws:/root/.aws \\
    "${DOCKER_TAG}"
_EOF
