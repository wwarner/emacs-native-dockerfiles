#! /bin/bash

cd "$(dirname "$(realpath "$0")")"/../emacs-native

USAGE="build.sh <mode> [mode]"
if [ "$#" == "0" ];
then
    echo "$USAGE" > /dev/stderr
    exit 1
fi

MODES_AVAILABLE=($(ls modes))
MODES=("${MODES_AVAILABLE[@]}")

if [ "$1" != "all" ]
then
    MODES=("$@")
fi

set -eu -o pipefail

EMACS_BRANCH="${EMACS_BRANCH:=30.2}"
DOCKERFILE="Dockerfile"

# Top
cat > "$DOCKERFILE" <<EOF
# Copyright 2024 William Warner
# SPDX-License-Identifier: GPL-3.0-only
# file generated with build.sh $@
FROM alpine:3.23
ARG EMACS_BRANCH

RUN apk add --no-cache \
    autoconf \
    bash \
    build-base \
    ca-certificates \
    cmake \
    curl \
    emacs-x11-nativecomp \
    emacs-vterm \
    fzf \
    git \
    libvterm-dev \
    ncurses \
    openssh-client \
    ripgrep \
    w3m \
    wget

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
