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

FP=$(shell printf '%07d' $(shell git rev-list --count --no-merges HEAD))-$(shell git rev-parse --short HEAD)
ARCH=$(shell uname -m)
BASE=${FP}-${ARCH}

emacs-native:
	docker build --progress plain -f emacs-native/Dockerfile -t emacs-native -t wwarner/emacs-native:${FP}-${ARCH} ./emacs-native

emacs-gopy: emacs-native
	docker build --progress plain -f emacs-gopy/Dockerfile --build-arg BASE=${BASE} -t emacs-gopy -t wwarner/emacs-gopy:${FP}-${ARCH} ./emacs-gopy

push: emacs-gopy
	docker push wwarner/emacs-native:${FP}-${ARCH}
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
