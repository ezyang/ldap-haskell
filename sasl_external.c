#include <ldap.h>
#include <sasl/sasl.h>

struct external_defaults {
  const char *authzPtr;
  int authzLen;
};

static int external_interact (LDAP *ld, unsigned flags, void *defaults, void *sasl_interact)
{
  (void)ld;
  (void)flags;
  struct external_defaults *defs = defaults;
  sasl_interact_t *interact;

  for (interact = sasl_interact; interact->id != SASL_CB_LIST_END; interact++) {
    switch (interact->id) {
    case SASL_CB_USER:
      if (defs->authzLen) {
        interact->result = defs->authzPtr;
        interact->len = defs->authzLen;
      }
      break;
    /* RFC 4422 (SASL) doesn't allow any other callbacks for EXTERNAL */
    }
  }
  return LDAP_SUCCESS;
}

int external_sasl_bind (LDAP *ld, const char *authz, int len)
{
  struct external_defaults defaults = { authzPtr: authz, authzLen: len };

  return ldap_sasl_interactive_bind_s (ld, NULL, "EXTERNAL", NULL, NULL,
                                       LDAP_SASL_QUIET,
                                       external_interact, &defaults);
}
