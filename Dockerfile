FROM matsen/docker-builder

COPY . /build
CMD cd /build && \
    make test && \
    make zip && \
    cp pplacer-linux.zip /export
