# Copyright 2024 William Warner
# SPDX-License-Identifier: GPL-3.0-only

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
RUN emacs -Q --script ".emacs.d/init.el"

CMD ["emacs"]
