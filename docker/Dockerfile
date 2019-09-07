# We could use one of the nomadiclab's docker images as a base instead
# We're using 4.06 instead of 4.06.1, if this causes problems build a custom 4.06.1 image instead
FROM ocaml/opam2:4.06

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

# Creat opam switch
RUN sh scripts/setup_switch.sh

RUN opam update

# Install ligo
RUN sh scripts/install_vendors_deps.sh
RUN sh -c pwd
RUN sh scripts/build_ligo_local.sh
RUN sh -c pwd
RUN ls -l
RUN sh -c 'ls -l /ligo/_build/default/src/bins/ligo || true'

# Use the ligo binary as a default command
ENTRYPOINT [ "/ligo/_build/default/src/bins/ligo" ]
