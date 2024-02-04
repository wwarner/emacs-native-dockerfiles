FROM golang:1.21-bookworm as emacs-src

WORKDIR /opt
RUN git clone --depth 1 --branch emacs-29 https://git.savannah.gnu.org/git/emacs.git

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
    cmake

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

COPY .emacs.d .emacs.d
RUN mkdir .emacs.d/data
RUN .emacs.d/postinstall.sh

COPY xterm-24bit.terminfo /tmp/xterm-24bit.terminfo
RUN /usr/bin/tic -x -o ~/.terminfo /tmp/xterm-24bit.terminfo
ENV TERM=xterm-24bit
ENV LC_ALL=C.utf8
ENV LANG=C.UTF-8
ENV LANGUAGE=C.UTF-8

CMD ["emacs"]