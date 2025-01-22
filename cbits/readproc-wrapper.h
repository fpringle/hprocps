#ifndef HPROCPS_READPROC_WRAPPER
#define HPROCPS_READPROC_WRAPPER

#include <proc/readproc.h>

PROCTAB *openproc_wrapper_simple(int flags);
PROCTAB *openproc_wrapper_pids(int flags, pid_t *pids);
PROCTAB *openproc_wrapper_uids(int flags, uid_t *uids, int nuid);

void closeproc_wrapper(PROCTAB* PT);

#endif
