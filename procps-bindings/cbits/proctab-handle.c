#include "proctab-handle.h"
#include <proc/readproc.h>

proctab_t *openproctab_simple(int flags) {
  flags = (flags & (~PROC_PID)) & (~PROC_UID);
  PROCTAB *PT = openproc(flags);
  return (proctab_t *)PT;
}

proctab_t *openproctab_pids(int flags, pid_t *pids) {
  flags = (flags | PROC_PID) & (~PROC_UID);
  PROCTAB *PT = openproc(flags, pids);
  return (proctab_t *)PT;
}

proctab_t *openproctab_uids(int flags, uid_t *uids, int nuid) {
  flags = (flags | PROC_UID) & (~PROC_PID);
  PROCTAB *PT = openproc(flags, uids, nuid);
  return (proctab_t *)PT;
}

void closeproctab(proctab_t *pt) {
  if (pt) {
    PROCTAB *PT = (PROCTAB *)pt;
    closeproc(PT);
  }
}

proc_t_wrapper **readallprocs_simple(int flags) {
  flags = (flags & (~PROC_PID)) & (~PROC_UID);
  proc_t **P = readproctab(flags);
  return (proc_t_wrapper **)P;
}

proc_t_wrapper **readallprocs_pids(int flags, pid_t *pids) {
  flags = (flags | PROC_PID) & (~PROC_UID);
  proc_t **P = readproctab(flags, pids);
  return (proc_t_wrapper **)P;
}

proc_t_wrapper **readallprocs_uids(int flags, uid_t *uids, int nuid) {
  flags = (flags | PROC_UID) & (~PROC_PID);
  proc_t **P = readproctab(flags, uids, nuid);
  return (proc_t_wrapper **)P;
}
