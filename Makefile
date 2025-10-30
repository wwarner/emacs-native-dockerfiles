.PHONY: arm64 x86_64 emacs-native push manifests

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

EMACS_BRANCH=30.2
FP=v${EMACS_BRANCH}-$(shell printf '%04d' $(shell git rev-list --count --no-merges HEAD))-$(shell git rev-parse --short HEAD)

emacs-native: arm64 x86_64

arm64 x86_64:
	ARCH=$@ ./build/build.sh all
	docker tag emacs-native:${FP}-$@ wwarner/emacs-native:${FP}-$@

push: emacs-native
	docker push wwarner/emacs-native:${FP}-arm64
	docker push wwarner/emacs-native:${FP}-x86_64

manifests:
	docker pull wwarner/emacs-native:${FP}-arm64
	docker pull wwarner/emacs-native:${FP}-x86_64
	docker manifest create wwarner/emacs-native:${FP} \
	       --amend wwarner/emacs-native:${FP}-arm64 \
               --amend wwarner/emacs-native:${FP}-x86_64
	docker manifest push wwarner/emacs-native:${FP}
	docker manifest rm wwarner/emacs-native:latest
	docker manifest create wwarner/emacs-native:latest \
	       --amend wwarner/emacs-native:${FP}-arm64 \
               --amend wwarner/emacs-native:${FP}-x86_64
	docker manifest push wwarner/emacs-native:latest
