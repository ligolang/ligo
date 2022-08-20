FROM node:14-alpine

RUN apk add --no-cache docker-cli

RUN npm -g config set user root && npm i -g truffle@5.4.3 && npm cache clean --force
