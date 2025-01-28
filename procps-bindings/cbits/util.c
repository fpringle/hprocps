#include "util.h"
#include <stdarg.h>
#include <stdio.h>

int xprintf(const char *format, ...) {
#ifdef DEBUG
  va_list args;
  va_start(args, format);

  int ret = printf(format, args);

  va_end(args);

  return ret;
#else
  return 0;
#endif
}
