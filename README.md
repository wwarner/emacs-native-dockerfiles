Dockerfiles for Emacs With Native Compilation
=============================================

Native Compilation, 24 bit color, LSP, Gopls, Pyright, Ripgrep, Fzf,
Treemacs, HideShow and more. Eliminates trial and error. Runs the same
everywhere!

## Installation

Run directly from the image at dockerhub:

	docker run -it --rm --name emacs \
	  -v$HOME/src:/root/src \
	  -v$HOME/.gitconfig:/etc/gitconfig \
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
formatting. For Go, `gopls`, `gofumpt` and `dlv` are installed where
emacs can find them. For Python, `pyright` is installed.

## No X11

I have been running emacs without windows for several years now
because it's so fast and portable. In particular, when working with a
lot of data, I find that I need to run emacs on a server in the
cloud. Somewhere along the line, I realized that emacs is a lot more
fun without graphics. The compilation step in this build is also much
much faster `--without-x11`. If you want your graphics back, then just
drop that configuration flag.

## 24bit Color & Unicode

24 bit color allows modern themes like zenburn and solarized to work
well. Unicode is configured correctly.

## Fast Start

All elisp is loaded and compiled in the build image, so that emacs
starts very quickly.

### Frequently Used Keys

* Cycle through windows with `M-o`
* Cycle back and forth through buffers with `M-n` and `M-p`
* Kill buffer with `M-K`
* Delete window with `M-0`
* Edit all occurances of the current token in the buffer with `C-c ;` (iedit)
* Comment code with `M-;`; repeat to uncomment
* Collapse code block with `C-c <down>`; repeat to show it again

### Wgrep

Search and replace across many files with
[wgrep](https://www.reddit.com/r/emacs/comments/u6yibf/if_you_have_never_used_wgrep_with_rgel_to_rename/). Find
and edit file names with
[wdired](https://www.masteringemacs.org/article/wdired-editable-dired-buffers).

### Git Configuration

This docker image is designed to provide an emacs development
environment that works equally well on Mac and Linux
machines. However, it works a bit better on Macs because Docker for
Desktop is designed to support containerized development while docker
on Linux is not. If you run this emacs container on Linux (as I do),
you will get a "dubious ownership" error from git unless you configure
git to recognize the mounted directories as safe with:

    git config --global --add safe.directory /container/path/to/workspace

You can also edit ~/.gitconfig and declare all directories safe

    [safe]
    	directory = *

For commiting to github, you'll need a user stanza as well, so that
your commits can be mapped to your github email.

    [user]
		name = wenjie
    	email = 9873498+wenjie9489@users.noreply.github.com

Either way, mount ~/.gitconfig to /etc/gitconfig to allow git to work
inside the container.

In addition to all of that, git needs your ssh keys for all
communication with git forges (Github, Gitlab), so those need to be
mounted to /root/.ssh as well.

## References

[docker-emacs](https://github.com/Silex/docker-emacs) (not compiled
with --native-comp or for `arm64` so decided to start fresh)

[EmacsPlus](https://github.com/d12frosted/homebrew-emacs-plus/blob/master/Formula/emacs-plus%4030.rb)

[Mastering Emacs](https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation)

[Spacemacs Terminal](https://github.com/troyp/spacemacs/wiki/Terminal)
