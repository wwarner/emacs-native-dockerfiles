#! /bin/bash

cd "$(dirname "$(realpath "$0")")"/../modes

USAGE="build.sh <mode> [mode]"
if [ "$#" == "0" ];
then
    echo "$USAGE" > /dev/stderr
    exit 1
fi

MODES_AVAILABLE=($(ls))
MODES=("$@")
if [ "$1" == "all" ]
then
    MODES=("${MODES_AVAILABLE[@]}")
fi
set -eu -o pipefail
DOCKER_TAG=emacs-$(printf '%s-' "${MODES[@]}") # join args
DOCKER_TAG=${DOCKER_TAG%-} # Remove trailing dash
DOCKER_TAG=$(echo "$DOCKER_TAG" | tr '/_' '-' | tr '[:upper:]' '[:lower:]')

EMACS_BRANCH="${EMACS_BRANCH:=30.1}"
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
FROM wwarner/emacs-native:${BASE}

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
