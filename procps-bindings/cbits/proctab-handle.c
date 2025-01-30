#include "proctab-handle.h"
#include "util.h"
#include <proc/readproc.h>
#include <stdint.h>

proctab_t *openproctab_simple(int flags) {
  xprintf("[C] openproctab_simple(%d);\n", flags);
  flags = (flags & (~PROC_PID)) & (~PROC_UID);
  PROCTAB *PT = openproc(flags);
  xprintf("[C]   return %p;\n\n", PT);
  return (proctab_t *)PT;
}

proctab_t *openproctab_pids(int flags, pid_t *pids) {
  xprintf("[C] openproctab_pids(%d, %p);\n", flags, pids);
  flags = (flags | PROC_PID) & (~PROC_UID);
  PROCTAB *PT = openproc(flags, pids);
  xprintf("[C]   return %p;\n\n", PT);
  return (proctab_t *)PT;
}

proctab_t *openproctab_uids(int flags, uid_t *uids, int nuid) {
  xprintf("[C] openproctab_uids(%d, %p, %d);\n", flags, uids,
          nuid);
  flags = (flags | PROC_UID) & (~PROC_PID);
  PROCTAB *PT = openproc(flags, uids, nuid);
  xprintf("[C]   return %p;\n\n", PT);
  return (proctab_t *)PT;
}

void closeproctab(proctab_t *pt) {
  xprintf("[C] closeproctab(%p);\n", pt);
  PROCTAB *PT = (PROCTAB *)pt;
  closeproc(PT);
}

proc_t_wrapper **readallprocs_simple(int flags) {
  xprintf("[C] readallprocs_simple(%d);\n", flags);
  flags = (flags & (~PROC_PID)) & (~PROC_UID);
  proc_t **P = readproctab(flags);
  xprintf("[C]   return %p;\n\n", P);
  return (proc_t_wrapper **)P;
}

proc_t_wrapper **readallprocs_pids(int flags, pid_t *pids) {
  xprintf("[C] readallprocs_pids(%d, %p);\n", flags, pids);
  flags = (flags | PROC_PID) & (~PROC_UID);
  proc_t **P = readproctab(flags, pids);
  xprintf("[C]   return %p;\n\n", P);
  return (proc_t_wrapper **)P;
}

proc_t_wrapper **readallprocs_uids(int flags, uid_t *uids, int nuid) {
  xprintf("[C] readallprocs_uids(%d, %p, %d);\n", flags, uids,
          nuid);
  flags = (flags | PROC_UID) & (~PROC_PID);
  proc_t **P = readproctab(flags, uids, nuid);
  xprintf("[C]   return %p;\n\n", P);
  return (proc_t_wrapper **)P;
}
