#!/bin/bash

# Default image name
DEFAULT_IMAGE="pplacer:latest"
# Use first argument if provided, else default
IMAGE_NAME="${1:-$DEFAULT_IMAGE}"

# run outside docker container
sudo docker run -it \
    --env DISPLAY=$DISPLAY \
    --env XAUTHORITY=$XAUTHORITY \
    --volume /tmp/.X11-unix:/tmp/.X11-unix \
    --volume "$(pwd)":/data/ \
    "$IMAGE_NAME"

# run inside docker container
#apt install -y python3-sphinx texlive texlive-latex-extra dvipng
#eval $(opam config env)
#make docs

