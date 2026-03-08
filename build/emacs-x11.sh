xhost +localhost
docker run --rm --name emacs-x11 -e DISPLAY=host.docker.internal:0 \
       -v$PWD:/src -v$HOME/.ssh:/root/.ssh -v$HOME/src:/root/src \
       -v$HOME/.aws:/root/.aws -v$HOME/.gitconfig:/etc/gitconfig \
       -it wwarner/emacs-native:latest
