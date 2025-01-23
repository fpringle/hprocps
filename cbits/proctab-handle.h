#ifndef HPROCPS_PROCTAB_HANDLE
#define HPROCPS_PROCTAB_HANDLE

typedef struct proctab_t proctab_t;

#include "proc-handle.h"
#include <proc/readproc.h>

proctab_t *openproctab_simple(int flags);
proctab_t *openproctab_pids(int flags, pid_t *pids);
proctab_t *openproctab_uids(int flags, uid_t *uids, int nuid);

void closeproctab(proctab_t *PT);

proc_t_wrapper **readproctab_simple(int flags);
proc_t_wrapper **readproctab_pids(int flags, pid_t *pids);
proc_t_wrapper **readproctab_uids(int flags, uid_t *uids, int nuid);

#endif
