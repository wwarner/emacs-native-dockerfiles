.PHONY: emacs-native emacs-gopy

build: emacs-native emacs-gopy

emacs-native:
	docker build --progress plain -f emacs-native/Dockerfile -t emacs-native ./emacs-native

emacs-gopy: emacs-native
	docker build --progress plain -f emacs-gopy/Dockerfile -t emacs-gopy ./emacs-gopy
