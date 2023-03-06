# syntax=docker/dockerfile:1.4
# To run the build on this image, you have to change the build context for ../../../ with the following command 
# docker buildx build --build-context syntaxes=../../../lsp/vscode-plugin/syntaxes .
# Because this project is using some file from ../../../lsp/vscode-plugin/syntaxes through symlink.

# Here symlink gonna be replaced by the files themself

ARG BUILDER_IMAGE="node:16"
ARG RUNNER_IMAGE="joseluisq/static-web-server:2.9-alpine"

FROM ${BUILDER_IMAGE} as builder

WORKDIR /app

ENV NODE_OPTIONS=--max_old_space_size=4096

COPY . .

RUN yarn install

ENV MEASUREMENT_ID="G-5H01XTN4PY"
ENV GIT_PROXY="https://ligo-webide-cors-proxy.serokell.team"
ARG backend_url
ARG http_protocol
ENV HTTP_PROTOCOL=$http_protocol
ENV BACKEND_URL=$backend_url
COPY --from=syntaxes . ./src/ligo-components/eth-project/Project/languages/syntaxes/
RUN echo $(ls ./src/ligo-components/eth-project/Project/languages/syntaxes/)
RUN yarn build:react-prod

FROM ${RUNNER_IMAGE}

LABEL maintainer="infra@marigold.dev"
ENV NODE_ENV production
ENV SERVER_FALLBACK_PAGE /public/index.html
COPY --from=builder /app/build /public

EXPOSE 3000

ENTRYPOINT ["static-web-server", "--root", "/public" , "-g", "INFO" ,"-p" , "3000"]
