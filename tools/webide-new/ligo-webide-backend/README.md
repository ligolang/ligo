# ligo-webide

To build and run the backend server:
```
stack build
stack exec ligo-webide-backend -- \
    --ligo-path /path/to/ligo/executable
```

To run the tests
```
LIGO_PATH=/path/to/ligo/executable
stack build --test
```
