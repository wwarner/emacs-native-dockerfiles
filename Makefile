.PHONY: emacs-native emacs-gopy

# The first two stanzas build to the local architecture
emacs-native:
	docker build --progress plain -f emacs-native/Dockerfile -t emacs-native ./emacs-native

emacs-gopy:
	docker build --progress plain -f emacs-gopy/Dockerfile -t emacs-gopy ./emacs-gopy

# All that follows is for building and then pushing the images to
# dockerhub. The build statements don't work properly, and just ignore
# the `--platform` flag.
#
# To actually create images, build arm on an M1, build amd on an ec2
# host. Push the tagged images to dockerhub. Then on either host, pull
# down all the images, for every arch. From there, create and then
# push the manifest.

emacs-native-amd64:
	docker build --platform linux/amd64 --progress plain -f emacs-native/Dockerfile -t wwarner/emacs-native:latest-amd64 ./emacs-native

emacs-native-arm64:
	docker build --platform linux/arm64 --progress plain -f emacs-native/Dockerfile -t wwarner/emacs-native:latest-arm64 ./emacs-native

emacs-gopy-amd64:
	docker build --platform linux/amd64 --progress plain -f emacs-gopy/Dockerfile -t wwarner/emacs-gopy:latest-amd64 ./emacs-gopy

emacs-gopy-arm64:
	docker build --platform linux/arm64 --progress plain -f emacs-gopy/Dockerfile -t wwarner/emacs-gopy:latest-arm64 ./emacs-gopy

build: emacs-native-amd64 emacs-native-arm64 emacs-gopy-amd64 emacs-gopy-arm64

push: build
	docker push wwarner/emacs-native:latest-amd64
	docker push wwarner/emacs-native:latest-arm64
	docker push wwarner/emacs-gopy:latest-amd64
	docker push wwarner/emacs-gopy:latest-arm64

manifest: push
	docker pull wwarner/emacs-native:latest-arm64
	docker pull wwarner/emacs-native:latest-amd64
	docker manifest create wwarner/emacs-native:latest \
	       --amend wwarner/emacs-native:latest-arm64 \
               --amend wwarner/emacs-native:latest-amd64
	docker manifest push wwarner/emacs-native:latest
	docker pull wwarner/emacs-gopy:latest-arm64
	docker pull wwarner/emacs-gopy:latest-amd64
	docker manifest create wwarner/emacs-gopy:latest \
	       --amend wwarner/emacs-gopy:latest-arm64 \
               --amend wwarner/emacs-gopy:latest-amd64
	docker manifest push wwarner/emacs-gopy:latest
