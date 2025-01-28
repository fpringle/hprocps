#include "proc-handle.h"
#include "util.h"
#include <proc/readproc.h>
#include <stdint.h>
#include <stdlib.h>

proc_t_wrapper *read_proc_wrapper(proctab_t *pt, proc_t_wrapper *p_init) {
  xprintf("[C] read_proc_wrapper(%p, %p);\n", pt,
          p_init);
  xprintf("[C]   proctab_t *pt          = %p\n", pt);
  xprintf("[C]   proc_t_wrapper *p_init = %p\n", p_init);

  if (pt) {
    PROCTAB *PT = (PROCTAB *)pt;
    proc_t *P_INIT = (proc_t *)p_init;

    xprintf("[C]   PROCTAB *PT            = %p\n", PT);
    xprintf("[C]   proc_t *P_INIT         = %p\n", P_INIT);

    proc_t *P = readproc(PT, P_INIT);
    proc_t_wrapper *p = (proc_t_wrapper *)P;

    xprintf("[C]   proc_t *P              = %p\n", P);
    xprintf("[C]   proc_t_wrapper *p      = %p\n", p);

    xprintf("[C]   return %p;\n\n", p);
    return p;
  } else {
    return 0;
  }
}

void free_proc_wrapper(proc_t_wrapper *p) {
  xprintf("[C] free_proc_wrapper(%p);\n", p);
  proc_t *P = (proc_t *)(p);
  freeproc(P);
}

proc_t_wrapper *lookup_self_wrapper() {
  xprintf("[C] lookup_self_wrapper();\n");
  proc_t *P = malloc(sizeof(proc_t));
  look_up_our_self(P);
  proc_t_wrapper *p = (proc_t_wrapper *)P;
  xprintf("[C]   return %p;\n\n", p);
  return p;
}
