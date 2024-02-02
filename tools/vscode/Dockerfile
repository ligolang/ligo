FROM node:18-buster

# Install vscode runtime dependencies that will later be used by vscode downloaded separately.
# Extest doesn't work with vscode installed this way for some reason >:(

ARG VSCODE_VERSION="1.81.1"
RUN wget -q https://update.code.visualstudio.com/${VSCODE_VERSION}/linux-deb-x64/stable -O code.deb
RUN apt-get -y update && apt install libffi-dev && apt-get -y install ./code.deb && apt-get -y remove code

# To avoid downloading vscode and chrome driver every time
RUN mkdir -p /vscode/.vscode-test/ /vscode/test-resources/VSCode-linux-x64
# Needed for extest-based tests
RUN wget -q https://chromedriver.storage.googleapis.com/108.0.5359.71/chromedriver_linux64.zip
RUN unzip chromedriver_linux64.zip -d /vscode/test-resources/
RUN wget -q https://update.code.visualstudio.com/${VSCODE_VERSION}/linux-x64/stable -O stable.tar.gz
RUN tar -xf stable.tar.gz -C /vscode/test-resources/
RUN ln -s /vscode/test-resources/VSCode-linux-x64 /vscode/.vscode-test/vscode-linux-x64-${VSCODE_VERSION}
# Needed for running tests in headless mode
RUN apt-get -y install xvfb xclip


ARG OCTEZ_VERSION="v16.1"
RUN wget -q https://github.com/serokell/tezos-packaging/releases/download/${OCTEZ_VERSION}-1/octez-client -O /usr/bin/octez-client
RUN chmod +x /usr/bin/octez-client

# Some runtime dependency for ligo
RUN yarn global add esy

COPY ligo /usr/bin/
RUN chmod +x /usr/bin/ligo

ENV USER vscode
RUN useradd --create-home vscode
COPY ./tools/vscode /vscode

# Broken symlink
RUN rm /vscode/LICENSE.md && touch /vscode/LICENSE.md

RUN mkdir /vscode/bin
COPY ./Linux/bin/ligo-debugger /vscode/bin
RUN chmod +x /vscode/bin/ligo-debugger

# To avoid running vscode as a root
RUN chown -R vscode:vscode /vscode
WORKDIR /vscode
