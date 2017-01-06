#include <ldap.h>
#include "sasl_bind.h"

static int
sasl_trivial_interact (LDAP * ld, unsigned flags, void *defaults,
                       void *sasl_interact)
{
  (void) ld;
  (void) flags;
  (void) defaults;
  (void) sasl_interact;
  return LDAP_SUCCESS;
}

int
trivial_external_sasl_bind (LDAP * ld)
{
  return ldap_sasl_interactive_bind_s (ld, NULL, "EXTERNAL", NULL, NULL,
                                       LDAP_SASL_QUIET,
                                       sasl_trivial_interact, NULL);
}
