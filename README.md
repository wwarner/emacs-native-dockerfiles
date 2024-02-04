Dockerfiles for Emacs With Native Compilation
=============================================

Use Native Compilation, LSP, Gopls, Pyright, Ripgrep, Fzf, Treemacs
and HideShow with less trial and error. Runs the same everywhere!

## Installation

    git clone git@github.com:wwarner/emacs-native-dockerfiles.git
	cd emacs-native-dockerfiles
	make # takes about 30 minutes on my laptop
	docker run -it --rm --name emacs \
	  -v$HOME/src:/root/src \
	  -v$HOME/.gitconfig:/root/.gitconfig \
	  -v$HOME/.ssh:/root/.ssh \
	  -v$HOME/.aws:/root/.aws \
	  emacs-gopy

## Native Compliation

Emacs lisp can be compiled to native machine code when built
`--with-native-compilation` enabled, resulting in a perceptible
performance improvement. There are several OS level dependencides
required by native compilation that are captured in this Dockerfile,
including libgccjit and gnutls.

## Native JSON with libjansson

Native json support is also supported by enabling `--with-json` and
its libjansson dependencies.

## LSP

I currently work in Go and Python, and I've installed the dependencies
needed for syntax highlighting, auto-completion and code
formatting.

For Go, `gopls` is the language server, `gofumpt` for code formatting
and `dlv` for source level debugging.

For Python, a virtual environment is installed and `pyright` is used
for the language server.

## No X11

I have been running emacs without windows for several years now
because it's so fast and portable. In particular, when working with a
lot of data, I find that I need to run my editor on a server in the
cloud. Somewhere along the line, I realized that Emacs is a lot more
fun without graphics. The compilation step in this build is also much
much faster `--without-x11`. If you want your graphics back, then just
drop that configuration flag.

### Frequently Used Keys

* Cycle through windows with M-o
* Cycle back and forth through buffers with M-n and M-p
* Kill buffer with M-K
* Delete window with M-0
* Edit all occurances of the current token in the buffer with C-c ; (iedit)
* Fuzzy find files with M-Z (fzf)
* Coment code with C-c C-c, and uncomment with C-c C-v
* Collapse code block with C-c <down>; repeat to show it again

## References

[docker-emacs](https://github.com/Silex/docker-emacs) (not compiled
with --native-comp or for `arm64` so decided to start fresh)

[EmacsPlus](https://github.com/d12frosted/homebrew-emacs-plus/blob/master/Formula/emacs-plus%4030.rb)

[Mastering Emacs](https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation)
