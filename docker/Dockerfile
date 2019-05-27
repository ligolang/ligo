# We could use one of the nomadiclab's docker images as a base instead
# We're using 4.06 instead of 4.06.1, if this causes problems built a custom 4.06.1 image instead
FROM ocaml/opam2:4.06

USER root

# Setup a custom opam repository where ligo is published
RUN git clone https://gitlab.com/ligolang/ligo.git
RUN opam repo add ligo-opam-repository ./ligo/vendors/ligo-opam-repository-local-generated/
RUN opam update ligo-opam-repository

# Install required native dependencies
RUN apt-get -y install \ 
    libev-dev \
    perl \
    pkg-config \
    libgmp-dev \
    libhidapi-dev \
    m4

# Install ligo
RUN git clone https://gitlab.com/gabriel.alfour/ligo
RUN opam update

RUN cd ligo/src && \
    opam install . --yes

RUN eval $(opam env)

# Use the ligo binary as a default command
ENTRYPOINT [ "/home/opam/.opam/4.06/bin/ligo" ]
