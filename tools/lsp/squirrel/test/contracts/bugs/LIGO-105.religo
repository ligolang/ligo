#ifdef DEBUG
#elif defined VERBOSE
#else
#endif

#include "file"

#define FOO
#define FOO "hello"

#undef FOO

#if VERSION == 2.0
  #error Unsupported
  #warning Not really supported
#endif
