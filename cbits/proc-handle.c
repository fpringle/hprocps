#include "proc-handle.h"
#include <proc/readproc.h>
#include <stdio.h>

proc_t_wrapper *read_proc_wrapper(proctab_t *pt, proc_t_wrapper *p_init) {
  if (pt) {
    PROCTAB *PT = (PROCTAB *)pt;
    proc_t *P_INIT = (proc_t *)p_init;
    proc_t *P = readproc(PT, P_INIT);
    proc_t_wrapper *p = (proc_t_wrapper *)P;
    return p;
  } else {
    return 0;
  }
}

void free_proc_wrapper(proc_t_wrapper *p) {
  if (p) {
    proc_t *P = (proc_t *)(p);
    freeproc(P);
  }
}
