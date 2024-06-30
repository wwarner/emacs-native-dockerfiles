# Copyright 2024 William Warner
# SPDX-License-Identifier: GPL-3.0-only

FROM debian:bookworm-slim

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update -y && \
    apt-get install -y \
    curl \
    gnupg \
    openssh-client \
    wget \
    git \
    fzf \
    ripgrep \
    build-essential \
    libssl-dev \
    libffi-dev \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg-agent \
    software-properties-common \
    gcc-12 \
    libgccjit0 \
    libgccjit-12-dev \
    libjansson4 \
    libjansson-dev \
    libgnutls28-dev \
    autoconf \
    texinfo \
    gnutls-bin \
    libncurses-dev \
    w3m \
    libtree-sitter-dev \
    cargo \
    zlib1g-dev \
    libvterm-dev \
    cmake \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt
RUN git clone --depth 1 --branch emacs-29 git://git.sv.gnu.org/emacs.git

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
