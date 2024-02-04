Dockerfiles for Emacs With Native Comp
======================================

Use Native Compilation, LSP, Gopls, Pyright, Ripgrep, Fzf, Treemacs
and HideShow with less trial and error.

## Native Compliation

Emacs lisp can be compiled to native machine code when built
`--with-native-compilation` enabled, resulting in a perceptible
performance improvement. There are several OS level dependencides
required by native compilation that are captured in ths Dockerfile,
including libgccjit and gnutls.

## Tree Sitter

Tree sitter is enabled by default in Emacs 29 and better, and care is
taken here to both enable it and install the necessary dependencies

## Native JSON with libjansson

Native json support is also supported by enabling `--with-json` and
its libjansson dependencies.

## LSP

I currently work in Go and Python, and I've installed the dependencies
needed for syntax highlighting, auto-completion and code
formatting.

For Go, `gopls` is installed for the language server, `gofumpt` for
code formatting and `dlv` for source level debugging.

For Python, a virtual environment is installed and `pyright` is used
for the language server.

## No X11

I have been running emacs without windows for several years now
because it's so portable. In particular, when working with a lot of
data, I find that I need to run my editor on a server in the
cloud. Somewhere along the line, I realized that Emacs is a lot more
fun, and certainly more portable, without graphics. The compilation
step in this build is also much much faster `--without-x11`. If you
want your graphics back, then just drop that configuration flag.

## Emacs

My init.el is a mish-mash of selcted packages, `use-package` and
`straight` installs. It's intended to provide utility without becoming
overly complex. I find most Emacs defaults acceptable, and I never
need more than a single file to create a pleasant experience. While
this tiny amount of customization fits easily into a single elisp
file, the supporting setup with its dependencies requires this
Dockerfile.

## Installation

    make
    docker run -v$HOME/.ssh:/root/.ssh -v$HOME/src:/root/src -it emacs-gopy

## References

[docker-emacs](https://github.com/Silex/docker-emacs) (not compiled
with --native-comp so had to start fresh)

[EmacsPlus](https://github.com/d12frosted/homebrew-emacs-plus/blob/master/Formula/emacs-plus%4030.rb)

[Mastering Emacs](https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation)
