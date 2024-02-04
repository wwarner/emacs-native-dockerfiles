ARG BASE
FROM wwarner/emacs-native:${BASE}

ENV DEBIAN_FRONTEND=noninteractive

COPY --from=golang:1.21-bookworm /usr/local/go/ /usr/local/go/
ENV PATH=/root/go/bin:/usr/local/go/bin:$PATH

RUN apt-get update && \
    apt-get install -y \
    python3 \
    python3-dev \
    python3-pip \
    python3-venv \
    python3-launchpadlib && \
    rm -rf /var/lib/apt/lists/*

RUN go install github.com/go-delve/delve/cmd/dlv@v1.21.2
RUN go install golang.org/x/tools/gopls@v0.14.2
RUN go install mvdan.cc/gofumpt@v0.5.0
ENV HOME /root
WORKDIR /root

ENV VENVDIR $HOME/.virtualenv
RUN python3 -m venv $VENVDIR
ENV PATH="$VENVDIR/bin:$PATH"
RUN pip install pyright

COPY .emacs.d .emacs.d

RUN emacs -Q --script ".emacs.d/init.el"

CMD ["emacs"]
