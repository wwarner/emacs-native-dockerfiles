.PHONY: emacs-native emacs-gopy

# To publish images, build arm on an M1, build amd on an ec2
# instance. Push the tagged images to dockerhub. Then on either host,
# pull down all the images, for every arch. From there, create and
# then push the manifest.
#
# i.e.
# on an arm host
# % make push
# now on an amd host
# $ make push manifests

EMACS_BRANCH=30.1
FP=v${EMACS_BRANCH}-$(shell printf '%04d' $(shell git rev-list --count --no-merges HEAD))-$(shell git rev-parse --short HEAD)
ARCH=$(shell uname -m)
BASE=${FP}-${ARCH}

emacs-native:
	docker build --build-arg EMACS_BRANCH=${EMACS_BRANCH} -f emacs-native/Dockerfile -t emacs-native -t wwarner/emacs-native:${FP}-${ARCH} ./emacs-native

emacs-gopy: emacs-native
	cd build
	./build/build.sh go py

push: emacs-gopy
	docker push wwarner/emacs-native:${FP}-${ARCH}
	docker tag emacs-go-py wwarner/emacs-gopy:${FP}-${ARCH}
	docker push wwarner/emacs-gopy:${FP}-${ARCH}

manifests:
	docker pull wwarner/emacs-native:${FP}-arm64
	docker pull wwarner/emacs-native:${FP}-x86_64
	docker manifest create wwarner/emacs-native:${FP} \
	       --amend wwarner/emacs-native:${FP}-arm64 \
               --amend wwarner/emacs-native:${FP}-x86_64
	docker manifest push wwarner/emacs-native:${FP}
	docker pull wwarner/emacs-gopy:${FP}-arm64
	docker pull wwarner/emacs-gopy:${FP}-x86_64
	docker manifest create wwarner/emacs-gopy:${FP} \
	       --amend wwarner/emacs-gopy:${FP}-arm64 \
               --amend wwarner/emacs-gopy:${FP}-x86_64
	docker manifest push wwarner/emacs-gopy:${FP}
	docker manifest rm wwarner/emacs-native:latest
	docker manifest create wwarner/emacs-native:latest \
	       --amend wwarner/emacs-native:${FP}-arm64 \
               --amend wwarner/emacs-native:${FP}-x86_64
	docker manifest push wwarner/emacs-native:latest
	docker manifest rm wwarner/emacs-gopy:latest
	docker manifest create wwarner/emacs-gopy:latest \
	       --amend wwarner/emacs-gopy:${FP}-arm64 \
               --amend wwarner/emacs-gopy:${FP}-x86_64
	docker manifest push wwarner/emacs-gopy:latest
