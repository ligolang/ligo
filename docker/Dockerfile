# At the moment, this really means 4.07.1
FROM ocaml/opam2:4.07

USER root

# Add contents of the current directory to /ligo where it can be
# accessed when building the image.
#
# This is useful when building either locally, or on the CI
# because the currently checkout out version (from git) will be used
# to build the image
ADD . /ligo

# Set the current working directory to /ligo for
# the upcoming scripts
WORKDIR /ligo

# Install required native dependencies
RUN sh scripts/install_native_dependencies.sh

# Install OPAM
# TODO: or scripts/install_build_environment.sh ?
RUN sh scripts/install_opam.sh

# Add tezos repository
RUN sh scripts/setup_repos.sh

RUN opam update

# Install ligo
RUN sh scripts/install_vendors_deps.sh
RUN opam install -y ./src

# Use the ligo binary as a default command
ENTRYPOINT [ "/home/opam/.opam/4.07/bin/ligo" ]
