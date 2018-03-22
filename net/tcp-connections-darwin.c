#include <sys/types.h>

#include <sys/socketvar.h>
#include <sys/sysctl.h>

#include <netinet/in.h>
#include <netinet/in_pcb.h>
#include <netinet/tcp_var.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void diesys(const char *msg) {
  fprintf(stderr, "%s: %s\n", msg, strerror(errno));
  exit(1);
}

static void print(const char *what, const struct in6_addr *in6_addr,
                  const struct in_addr *in_addr, unsigned int port) {
  const uint32_t *words = (uint32_t *)in6_addr;

  printf("%s = %08x %08x %08x %08x : %08x : %u\n", what, (unsigned int)words[0],
         (unsigned int)words[1], (unsigned int)words[2], (unsigned int)words[3],
         (unsigned int)in_addr->s_addr, port);
}

int main(void) {
  static const char mibvar[] = "net.inet.tcp.pcblist";
  static struct xinpgen *xigs;
  static struct xinpgen *xig;
  struct xtcpcb *xt;
  size_t len;

  if (sysctlbyname(mibvar, 0, &len, 0, 0) == -1) {
    diesys("sysctl");
  }
  if (!(xigs = realloc(xigs, len))) {
    diesys("realloc");
  }
  if (sysctlbyname(mibvar, xigs, &len, 0, 0) == -1) {
    diesys("sysctl");
  }
  xig = xigs;
  for (;;) {
    xig = (struct xinpgen *)((char *)xig + xig->xig_len);
    if (xig->xig_len <= sizeof(struct xinpgen)) {
      break;
    }
    xt = (struct xtcpcb *)xig;
    if (xt->xt_inp.inp_gencnt > xigs->xig_gen) {
      continue;
    }
    print("local ", &xt->xt_inp.in6p_laddr, &xt->xt_inp.inp_laddr,
          xt->xt_inp.inp_lport);
    print("remote", &xt->xt_inp.in6p_faddr, &xt->xt_inp.inp_faddr,
          xt->xt_inp.inp_fport);
    printf("uid    = %ld\n", (long)xt->xt_socket.so_uid);
    printf("\n");
  }
}
