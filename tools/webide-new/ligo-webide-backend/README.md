# ligo-webide

To build and run the backend server:
```
stack build
stack exec ligo-webide-backend -- \
    --port 8080 \
    --verbosity 2 \
    --ligo-path /path/to/ligo/executable \
    --octez-client-path /path/to/octez-client/executable \
    --gist-token <GitHub api token for creating Gists> \
    --workspace-prefix /directory/for/LSP/files
```

To build and run the backend server with a Dockerized LIGO:
```
stack build
stack exec ligo-webide-backend -- \
    --port 8080 \
    --verbosity 2 \
    --dockerized-ligo-version 0.60.0 \
    --octez-client-path /path/to/octez-client/executable \
    --gist-token <GitHub api token for creating Gists> \
    --workspace-prefix /directory/for/LSP/files
    --gist-token <gist token for creating gists>
```

To run the tests
```
LIGO_PATH=/path/to/ligo/executable \
  OCTEZ_CLIENT_PATH=/path/to/octez-client/binary \
  stack build --test
```
