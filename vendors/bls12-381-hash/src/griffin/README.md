# Griffin in C

This is a C implementation of Griffin for the scalar field of BLS12-381 using [blst](https://github.com/supranational/blst/).

## Compile

Here a simple program using the primitive:
```C
#include "blst.h"
#include "blst_misc.h"
#include "griffin.h"
#include <string.h>
#include <stdlib.h>

int main() {
  blst_fr *ctxt = malloc(sizeof(blst_fr) * GRIFFIN_STATE_SIZE);

  memset(ctxt + 0, 0, sizeof(blst_fr));
  memset(ctxt + 1, 0, sizeof(blst_fr));
  memset(ctxt + 2, 0, sizeof(blst_fr));

  griffin_apply_permutation(ctxt);

  return (0);
}
```

To compile, use:
```
gcc \
  -o main \
  griffin.c main.c \
  -lblst \
  -I$(pwd) \
  -I$OPAM_SWITCH_PREFIX/lib/bls12-381 \
  -L$OPAM_SWITCH_PREFIX/lib/bls12-381
```
