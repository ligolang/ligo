ARG BUILDER_IMAGE="node:16"
ARG RUNNER_IMAGE="joseluisq/static-web-server:2.9-alpine"

FROM ${BUILDER_IMAGE} as builder

WORKDIR /app
ENV NODE_OPTIONS=--max_old_space_size=2048

COPY . .

RUN yarn install

ENV MEASUREMENT_ID="G-5H01XTN4PY"
RUN yarn build:react-prod

FROM ${RUNNER_IMAGE}

ENV NODE_ENV production
ENV SERVER_FALLBACK_PAGE /public/index.html
COPY --from=builder /app/build /public

EXPOSE 3000

ENTRYPOINT ["static-web-server", "--root", "/public" , "-g", "INFO" ,"-p" , "3000"]
