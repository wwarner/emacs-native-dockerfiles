Dockerfiles for Emacs With Native Compilation
=============================================

Native Compilation, 24 bit color, LSP, Gopls, Pyright, Ripgrep, Fzf,
Treemacs, HideShow and more. Eliminates trial and error. Runs the same
everywhere!

## Installation

Run directly from the image at dockerhub:

	docker run -it --rm --name emacs \
	  -v$HOME/src:/root/src \
	  -v$HOME/.gitconfig:/root/.gitconfig \
	  -v$HOME/.ssh:/root/.ssh \
	  -v$HOME/.aws:/root/.aws \
	  wwarner/emacs-gopy:latest

Or build it locally

    git clone git@github.com:wwarner/emacs-native-dockerfiles.git
	cd emacs-native-dockerfiles
	make emacs-gopy
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

Remarks:

* Mount ~/.gitconfig to share your github email address and possibly
  to share git repositories on the host with the guest. On Mac, with
  Docker for Desktop, virtualization is set up such that user ids are
  the same in the host and the guest and magit works without adding
  directories to .gitconfig. However, on linux, or also on Mac using
  Colima, the guest user ids are different, and git will not open the
  repository unless the directory in the guest is added to .gitconfig.

* Build and push with [Docker BuildKit](https://docs.docker.com/engine/reference/commandline/buildx_build/).
  * It looks like this, but runs very very slowly when building the foreign arch.
  `docker buildx build --platform linux/amd64,linux/arm64/v8 --push -t emacs-native:latest --progress plain .`

## References

[docker-emacs](https://github.com/Silex/docker-emacs) (not compiled
with --native-comp or for `arm64` so decided to start fresh)

[EmacsPlus](https://github.com/d12frosted/homebrew-emacs-plus/blob/master/Formula/emacs-plus%4030.rb)

[Mastering Emacs](https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation)

[Spacemacs Terminal](https://github.com/troyp/spacemacs/wiki/Terminal)
