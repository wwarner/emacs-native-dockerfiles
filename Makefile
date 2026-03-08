.PHONY: docker arm64 amd64 build push latest

# make docker  - creates Dockerfile in emacs-native/
# make arm64   - builds local arm64 image
# make amd64   - builds local amd64 image
# make build   - builds both images, tags this host's arch as emacs-native:latest (then: docker run emacs-native)
# make push    - pushes both images and creates a single multi-arch manifest on Docker Hub
# make latest  - tags the Docker Hub manifest as :latest

EMACS_BRANCH ?= 30.2
FP = v$(EMACS_BRANCH)-$(shell printf '%04d' $(shell git rev-list --count --no-merges HEAD))-$(shell git rev-parse --short HEAD)
DOCKERFILE = emacs-native/Dockerfile
CONTEXT = emacs-native
ARCH = $(shell uname -m | sed 's/x86_64/amd64/;s/aarch64/arm64/')

build: arm64 amd64
	docker tag emacs-native:$(FP)-$(ARCH) emacs-native:latest

docker:
	./build/build.sh all

arm64 amd64: docker
	BUILDKIT_PROGRESS=plain docker build --platform linux/$@ \
		-t emacs-native:$(FP)-$@ \
		-f $(DOCKERFILE) $(CONTEXT)
	docker tag emacs-native:$(FP)-$@ wwarner/emacs-native:$(FP)-$@

push: build
	docker push wwarner/emacs-native:$(FP)-arm64
	docker push wwarner/emacs-native:$(FP)-amd64
	docker manifest create wwarner/emacs-native:$(FP) \
		--amend wwarner/emacs-native:$(FP)-arm64 \
		--amend wwarner/emacs-native:$(FP)-amd64
	docker manifest push wwarner/emacs-native:$(FP)

latest: push
	-docker manifest rm wwarner/emacs-native:latest
	docker manifest create wwarner/emacs-native:latest \
		--amend wwarner/emacs-native:$(FP)-arm64 \
		--amend wwarner/emacs-native:$(FP)-amd64
	docker manifest push wwarner/emacs-native:latest
