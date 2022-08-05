FROM node:12-alpine as builder

ARG EXAMPLES_DIR_SRC
ARG EXAMPLES_DIR_DEST=packages/client/examples

WORKDIR /app

COPY package.json package.json
COPY yarn.lock yarn.lock
COPY tsconfig.json tsconfig.json
COPY packages/client packages/client
COPY packages/server packages/server
COPY $EXAMPLES_DIR_SRC $EXAMPLES_DIR_DEST

ENV EXAMPLES_DIR=/app/$EXAMPLES_DIR_DEST

RUN yarn install
RUN yarn workspaces run build

FROM node:12-buster

WORKDIR /app

RUN apt-get update && apt-get -y install perl6 libev-dev perl pkg-config libgmp-dev libhidapi-dev m4 libcap-dev bubblewrap rsync

RUN curl https://gitlab.com/ligolang/ligo/-/jobs/2816932316/artifacts/raw/ligo.deb --output /tmp/ligo.deb && dpkg -i /tmp/ligo.deb && rm /tmp/ligo.deb

COPY --from=builder /app/packages/client/build /app/client/build
COPY --from=builder /app/node_modules /app/node_modules
COPY --from=builder /app/packages/server/dist/src /app/server/dist

ENV STATIC_ASSETS /app/client
ENV LIGO_CMD /usr/local/bin/ligo

ENTRYPOINT [ "node", "server/dist/index.js" ]
