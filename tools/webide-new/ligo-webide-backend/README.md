# ligo-webide

To build and run the backend server:
```
stack build
stack exec ligo-webide-backend -- \
    --ligo-path /path/to/ligo/executable
```

To build and run the backend server with a Dockerized LIGO:
```
stack build
stack exec ligo-webide-backend -- \
    --dockerized-ligo-version 0.50.0
```

To run the tests
```
LIGO_PATH=/path/to/ligo/executable
stack build --test
```
