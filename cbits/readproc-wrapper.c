#include "readproc-wrapper.h"
#include <proc/readproc.h>

PROCTAB *openproc_wrapper_simple(int flags) {
  flags = (flags & (~PROC_PID)) & (~PROC_UID);
  return openproc(flags);
}

PROCTAB *openproc_wrapper_pids(int flags, pid_t *pids) {
  flags = (flags | PROC_PID) & (~PROC_UID);
  return openproc(flags, pids);
}

PROCTAB *openproc_wrapper_uids(int flags, uid_t *uids, int nuid) {
  flags = (flags | PROC_UID) & (~PROC_PID);
  return openproc(flags, uids, nuid);
}

void closeproc_wrapper(PROCTAB *PT) { closeproc(PT); }
