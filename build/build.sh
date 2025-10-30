#! /bin/bash

cd "$(dirname "$(realpath "$0")")"/../emacs-native

USAGE="build.sh <mode> [mode]"
if [ "$#" == "0" ];
then
    echo "$USAGE" > /dev/stderr
    exit 1
fi

DOCKER_TAG_SUFF=""
MODES_AVAILABLE=($(ls modes))
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
BASE=${FP}-${ARCH}
DOCKERFILE=".Dockerfile-${DOCKER_TAG}"

# Top
cat > "$DOCKERFILE" <<EOF
# Copyright 2024 William Warner
# SPDX-License-Identifier: GPL-3.0-only
# file generated with build.sh $@
FROM debian:trixie-slim
ARG EMACS_BRANCH

COPY debian-backports.sources /etc/apt/sources.list.d/debian-backports.sources

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update -t trixie-backports -y && \
    apt-get install -t trixie-backports -y \
    apt-transport-https \
    autoconf \
    build-essential \
    ca-certificates \
    cmake \
    curl \
    emacs-nox \
    fzf \
    gcc-12 \
    git \
    libvterm-dev \
    ripgrep \
    w3m \
    wget \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /root

# configure 24 bit color and unicode
COPY xterm-24bit.terminfo /tmp/xterm-24bit.terminfo
RUN /usr/bin/tic -x -o ~/.terminfo /tmp/xterm-24bit.terminfo
ENV TERM=xterm-24bit
ENV LC_ALL=C.utf8
ENV LANG=C.UTF-8
ENV LANGUAGE=C.UTF-8

# vterm behaves better with this setting
ENV SHELL=bash

WORKDIR /root

COPY .emacs.d .emacs.d

EOF

# Middle
for d in "${MODES[@]}"; do
    part="modes/${d}/Dockerfile.part"
    { echo "#"; echo "# ${part}"; cat "$part"; } >> "${DOCKERFILE}"
done

# Bottom
cat >> "${DOCKERFILE}" <<EOF

RUN emacs -Q --script ".emacs.d/init.el"

CMD ["emacs"]
EOF

BUILDKIT_PROGRESS=plain docker build --platform "linux/${ARCH}" -t "${DOCKER_TAG}:${BASE}" -f "${DOCKERFILE}" .
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
