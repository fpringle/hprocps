#ifndef HPROCPS_PROC_HANDLE
#define HPROCPS_PROC_HANDLE

typedef struct proc_t_wrapper proc_t_wrapper;

#include "proctab-handle.h"
#include <proc/readproc.h>

proc_t_wrapper *read_proc_wrapper(proctab_t *pt, proc_t_wrapper *p);

void free_proc_wrapper(proc_t_wrapper *p);

proc_t_wrapper *lookup_self_wrapper();

#endif
