# Copyright 2024 William Warner
# SPDX-License-Identifier: GPL-3.0-only

FROM debian:bookworm-20250113-slim
ARG EMACS_BRANCH

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update -y && \
    apt-get install -y \
    apt-transport-https \
    autoconf \
    build-essential \
    ca-certificates \
    cargo \
    cmake \
    curl \
    fzf \
    gcc-12 \
    git \
    gnupg \
    gnupg-agent \
    gnutls-bin \
    libffi-dev \
    libgccjit-12-dev \
    libgccjit0 \
    libgnutls28-dev \
    libjansson4 \
    libjansson-dev \
    libncurses-dev \
    libssl-dev \
    libtree-sitter-dev \
    libvterm-dev \
    openssh-client \
    pkg-config \
    ripgrep \
    software-properties-common \
    texinfo \
    w3m \
    wget \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt
RUN git clone --depth 1 --branch emacs-${EMACS_BRANCH} git://git.sv.gnu.org/emacs.git

ENV CC="gcc-12"

WORKDIR /opt/emacs
RUN ./autogen.sh \
        && ./configure \
	--with-native-compilation \
	--with-json \
	--with-tree-sitter \
	--without-x11

RUN make && make install

WORKDIR /root

RUN /bin/rm -rf /opt/emacs

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
